defmodule Rebus.Message do
  @moduledoc """
  D-Bus message protocol implementation.

  This module implements the D-Bus message format as specified in the D-Bus specification.
  A message consists of a header and a body, where the header contains metadata about
  the message and the body contains the actual data being transmitted.

  ## Message Structure

  A D-Bus message has the following structure:
  - Header: Fixed signature "yyyyuua(yv)" containing endianness, type, flags, version,
    body length, serial, and header fields
  - Body: Variable content based on the message signature

  ## Message Types

  - `:method_call` - Method call message
  - `:method_return` - Method reply with returned data
  - `:error` - Error reply
  - `:signal` - Signal emission

  ## Header Fields

  - `:path` - Object path (required for METHOD_CALL and SIGNAL)
  - `:interface` - Interface name (optional for METHOD_CALL, required for SIGNAL)
  - `:member` - Method or signal name (required for METHOD_CALL and SIGNAL)
  - `:error_name` - Error name (required for ERROR)
  - `:reply_serial` - Serial of message being replied to (required for ERROR and METHOD_RETURN)
  - `:destination` - Target connection name (optional)
  - `:sender` - Sending connection name (optional, usually set by message bus)
  - `:signature` - Signature of message body (optional, defaults to empty, automatically added to header_fields when body is present)
  - `:unix_fds` - Number of Unix file descriptors (optional)

  Note: The signature is stored in `header_fields[:signature]` and can be accessed using `Rebus.Message.signature/1`.

  ## Unix file descriptors

  The `unix_fds` struct field is separate from the `:unix_fds` header count.
  Construct outbound messages with `fds: [fd, ...]`; `h` body values are their
  zero-based wire indexes. Inbound descriptors appear in `message.unix_fds`
  only on a `Rebus.call/3` reply, and the calling process then owns them. See
  [Unix file descriptor passing](unix_fds.html).

  ## Message Flags

  - `:no_reply_expected` - Don't expect a reply
  - `:no_auto_start` - Don't auto-start destination service
  - `:allow_interactive_authorization` - Allow interactive authorization

  ## Examples

      # A method call
      {:ok, message} =
        Rebus.Message.new(:method_call,
          path: "/com/example/Object",
          interface: "com.example.Interface",
          member: "Method",
          destination: "com.example.Service",
          body: [42, "hello"],
          signature: "is"
        )

      # A signal
      {:ok, message} =
        Rebus.Message.new(:signal,
          path: "/com/example/Object",
          interface: "com.example.Interface",
          member: "SignalName",
          body: ["value"],
          signature: "s"
        )

      # An error reply
      {:ok, message} =
        Rebus.Message.new(:error,
          error_name: "com.example.Error.Failed",
          reply_serial: 123,
          body: ["Error message"],
          signature: "s"
        )
  """

  use TypedStruct

  alias Rebus.Decoder
  alias Rebus.Encoder
  alias Rebus.Message.UnixFDs
  alias Rebus.ProtocolLimitError
  alias Rebus.ResourceLimitError
  alias Rebus.Signature
  alias Rebus.WireValue

  import Bitwise, only: [bor: 2, band: 2]

  @max_message_size 134_217_728
  @max_array_size 67_108_864
  @max_scalar_elements 1_000_000
  @max_unix_fds 16

  @typedoc "Message type"
  @type message_type :: :method_call | :method_return | :error | :signal

  @typedoc "Message flags"
  @type flag :: :no_reply_expected | :no_auto_start | :allow_interactive_authorization

  @typedoc "Header field keys"
  @type header_field ::
          :path
          | :interface
          | :member
          | :error_name
          | :reply_serial
          | :destination
          | :sender
          | :signature
          | :unix_fds

  @type construction_error ::
          :invalid_body
          | :invalid_flags
          | :invalid_header_fields
          | :invalid_signature
          | :invalid_type
          | :invalid_unix_fds
          | :invalid_version
          | :message_too_large
          | :resource_limit
          | :unix_fd_limit
          | {:invalid_header_field, header_field()}
          | {:missing_header_field, header_field()}
          | {:unknown_header_field, term()}
  @type encoding_error ::
          :invalid_body
          | :invalid_header_fields
          | :invalid_message
          | :invalid_unix_fds
          | :message_too_large
          | :resource_limit
          | :unix_fd_limit

  typedstruct enforce: true do
    @typedoc "D-Bus message structure"
    field :type, message_type()
    field :flags, [flag()]
    field :version, non_neg_integer()
    field :body_length, non_neg_integer()
    field :serial, non_neg_integer()
    field :header_fields, %{optional(header_field()) => term()}
    field :body, [term()]
    field :unix_fds, [Rebus.UnixFD.t()], default: []
  end

  # Message type constants
  @message_types %{
    1 => :method_call,
    2 => :method_return,
    3 => :error,
    4 => :signal
  }

  @type_codes Map.new(@message_types, fn {k, v} -> {v, k} end)
  @message_type_values Map.values(@message_types)

  # Exceptions the encoder and decoder raise on malformed input. Each caller
  # maps the whole set to one error reason.
  @encode_exceptions [ArgumentError, CaseClauseError, FunctionClauseError, KeyError, MatchError]
  @decode_exceptions [ArgumentError, CaseClauseError, FunctionClauseError, MatchError]

  # Message flag constants
  @flags %{
    0x1 => :no_reply_expected,
    0x2 => :no_auto_start,
    0x4 => :allow_interactive_authorization
  }

  @flag_codes Map.new(@flags, fn {k, v} -> {v, k} end)

  # Header field constants
  @header_fields %{
    1 => :path,
    2 => :interface,
    3 => :member,
    4 => :error_name,
    5 => :reply_serial,
    6 => :destination,
    7 => :sender,
    8 => :signature,
    9 => :unix_fds
  }

  @field_codes Map.new(@header_fields, fn {k, v} -> {v, k} end)

  # Header field types - what D-Bus type each header field should have
  @field_types %{
    path: "o",
    interface: "s",
    member: "s",
    error_name: "s",
    reply_serial: "u",
    destination: "s",
    sender: "s",
    signature: "g",
    unix_fds: "u"
  }

  # Required header fields for each message type
  @required_fields %{
    method_call: [:path, :member],
    method_return: [:reply_serial],
    error: [:error_name, :reply_serial],
    signal: [:path, :interface, :member]
  }

  @doc """
  The largest complete D-Bus message accepted from the wire, in bytes.

  This is the D-Bus protocol limit of 2^27 bytes and includes the fixed header,
  header fields, alignment padding, and body.
  """
  @spec max_message_size() :: pos_integer()
  def max_message_size, do: @max_message_size

  @doc """
  The largest D-Bus array payload accepted or emitted, in bytes.

  This is the D-Bus protocol limit of 2^26 bytes. It is distinct from the
  local scalar materialization cap exposed by `max_scalar_elements/0`.
  """
  @spec max_array_size() :: pos_integer()
  def max_array_size, do: @max_array_size

  @doc """
  The maximum number of fixed-width scalar array elements per encode or decode.

  This local safety cap is 1,000,000 elements. Encoding shares it cumulatively
  across every fixed-width scalar array in one encode operation; it is not a
  D-Bus wire-format limit.
  """
  @spec max_scalar_elements() :: pos_integer()
  def max_scalar_elements, do: @max_scalar_elements

  @doc """
  The maximum number of Unix file descriptors accepted in one message.

  This local bound applies to the D-Bus header count and the ancillary-data
  control buffer. It is deliberately lower than operating-system limits.
  """
  @spec max_unix_fds() :: pos_integer()
  def max_unix_fds, do: @max_unix_fds

  @doc false
  @spec validate_encoded_size(non_neg_integer(), non_neg_integer()) ::
          :ok | {:error, :message_too_large}
  def validate_encoded_size(header_fields_size, body_length)
      when is_integer(header_fields_size) and header_fields_size >= 4 and is_integer(body_length) and
             body_length >= 0 do
    case frame_geometry(header_fields_size - 4, body_length) do
      {:ok, _header_padded_length, _total_size} -> :ok
      {:error, :message_too_large} = error -> error
    end
  end

  def validate_encoded_size(_header_fields_size, _body_length), do: {:error, :message_too_large}

  @doc """
  Creates a new D-Bus message.

  ## Parameters

  - `type` - The message type (`:method_call`, `:method_return`, `:error`, `:signal`)
  - `opts` - Keyword list of options:
    - `:flags` - List of message flags (default: `[]`)
    - `:version` - Protocol version (default: `1`)
    - `:body` - Message body as list of values (default: `[]`)
    - `:signature` - Message body signature (default: auto-generated from body;
      `:infinity`, `:negative_infinity`, and `:nan` infer `d`)
    - `:fds` - Borrowed Unix file descriptors. Each `h` value in the body is
      an index into this list. Rebus never closes outbound descriptors.
    - Header fields like `:path`, `:interface`, `:member`, etc.

  ## Note

  The serial number is initialized to 1. The transport layer that dispatches
  the message assigns its own serial number before writing the frame.

  ## Examples

      iex> {:ok, message} = Rebus.Message.new(:method_call,
      ...>   path: "/com/example/Object",
      ...>   member: "TestMethod"
      ...> )
      iex> message.type
      :method_call

  ## Errors

  Returns `{:error, reason}` where `reason` is one of:
  - `:invalid_type` - the message type is not a D-Bus message type
  - `:invalid_flags` - the flags are not a list, or include an unknown flag
  - `:invalid_version` - the protocol version is unsupported
  - `:invalid_body` - the body is not a list, or its values cannot be encoded
    by the signature
  - `:invalid_signature` - the signature is not a binary, or is not a valid
    D-Bus type expression
  - `{:invalid_header_field, field}` - the value given for that header field
    is not valid
  - `{:missing_header_field, field}` - a header field required for the message
    type was not given
  - `:invalid_unix_fds` - the descriptors do not match the body
  - `:unix_fd_limit` - the message exceeds the Unix file descriptor limit
  - `:message_too_large` - the encoded message, or an array within it, exceeds
    the D-Bus size limit
  - `:resource_limit` - a local structural, nesting, or scalar materialization
    cap is exceeded
  """
  @spec new(message_type(), keyword()) :: {:ok, t()} | {:error, construction_error()}
  def new(type, opts \\ []) do
    with :ok <- validate_message_type(type),
         {:ok, flags} <- validate_flags(Keyword.get(opts, :flags, [])),
         {:ok, version} <- validate_version(Keyword.get(opts, :version, 1)),
         {:ok, body} <- validate_body(Keyword.get(opts, :body, [])),
         {:ok, signature} <- get_or_generate_signature(opts, body),
         :ok <- validate_signature_format(signature),
         {:ok, unix_fds} <- UnixFDs.extract_unix_fds(opts),
         {:ok, header_fields} <- extract_header_fields(opts),
         {:ok, validated_fields} <- validate_header_fields(header_fields),
         :ok <- validate_required_fields(type, header_fields),
         {:ok, body_length} <- calculate_body_length(body, signature),
         :ok <- UnixFDs.validate_unix_fd_indices(signature, body, unix_fds),
         {:ok, validated_fields} <- put_signature_field(validated_fields, signature),
         {:ok, validated_fields} <- UnixFDs.put_unix_fd_count(validated_fields, unix_fds),
         {:ok, header_fields_data} <- encode_header_fields(validated_fields, :little),
         :ok <- validate_encoded_size(IO.iodata_length(header_fields_data), body_length) do
      {:ok,
       %__MODULE__{
         type: type,
         flags: flags,
         version: version,
         body_length: body_length,
         serial: 1,
         header_fields: validated_fields,
         body: body,
         unix_fds: unix_fds
       }}
    else
      {:error, _} = error -> error
    end
  end

  @doc """
  Creates a new D-Bus message, raising on error.

  Same as `new/2` but raises `ArgumentError` instead of returning `{:error, reason}`.
  """
  @spec new!(message_type(), keyword()) :: t()
  def new!(type, opts \\ []) do
    case new(type, opts) do
      {:ok, message} ->
        message

      {:error, :invalid_body} ->
        raise ArgumentError, "body does not match signature #{inspect(new_signature(opts))}"

      {:error, reason} ->
        raise ArgumentError, new_error_message(reason)
    end
  end

  defp new_error_message(:message_too_large), do: "message exceeds the D-Bus size limit"

  defp new_error_message(:resource_limit),
    do:
      "message exceeds a local resource limit (fixed-width scalar arrays allow at most #{max_scalar_elements()} elements per encode)"

  defp new_error_message(:unix_fd_limit), do: "message exceeds the Unix file descriptor limit"

  defp new_error_message(:invalid_unix_fds),
    do: "Unix file descriptors do not match the message body"

  defp new_error_message(:invalid_type), do: "invalid message type"
  defp new_error_message(:invalid_flags), do: "invalid message flags"
  defp new_error_message(:invalid_version), do: "unsupported D-Bus protocol version"
  defp new_error_message(:invalid_signature), do: "invalid message signature"
  defp new_error_message(:invalid_header_fields), do: "invalid header fields"

  defp new_error_message({:invalid_header_field, field}),
    do: "invalid value for header field #{inspect(field)}"

  defp new_error_message({:missing_header_field, field}),
    do: "missing required header field #{inspect(field)}"

  defp new_error_message({:unknown_header_field, field}),
    do: "unknown header field #{inspect(field)}"

  @doc """
  Encodes a message to iodata format.

  Returns the message encoded according to the D-Bus wire format specification.
  The endianness can be specified as `:little` or `:big` (default: `:little`).

  ## Parameters

  - `message` - The message to encode
  - `endianness` - Byte order (`:little` or `:big`, default: `:little`)

  ## Examples

      iex> message = Rebus.Message.new!(:signal, path: "/", interface: "org.example.Test", member: "Test")
      iex> {:ok, iodata} = Rebus.Message.encode(message)
      iex> is_binary(IO.iodata_to_binary(iodata))
      true

  ## Returns

  `{:ok, iodata}` on success. Returns `{:error, :invalid_body}` when the
  message body does not match its signature, `{:error, :invalid_header_fields}`
  for invalid header values, or `{:error, :invalid_message}` for an invalid
  fixed header or missing required fields. Returns `{:error, :message_too_large}`
  when the encoded frame exceeds the D-Bus message or header-fields limits, or
  an encoded array exceeds `max_array_size/0`, or `{:error, :resource_limit}`
  when a local structural, nesting, or scalar cap is exhausted.
  """
  @spec encode(t(), :little | :big) :: {:ok, iodata()} | {:error, encoding_error()}
  def encode(message, endianness \\ :little) when endianness in [:little, :big] do
    with :ok <- validate_encodable_envelope(message),
         :ok <- validate_encodable_header_fields(message.header_fields),
         {:ok, header_fields_encoded} <- encode_header_fields(message.header_fields, endianness),
         {:ok, body_data} <-
           encode_body(message.body, Map.get(message.header_fields, :signature, ""), endianness),
         :ok <- UnixFDs.validate_unix_fds(message),
         :ok <-
           validate_encoded_size(
             IO.iodata_length(header_fields_encoded),
             IO.iodata_length(body_data)
           ) do
      # Calculate actual body length
      body_length = IO.iodata_length(body_data)

      # Encode the fixed header
      endian_flag = if endianness == :little, do: ?l, else: ?B
      type_byte = Map.get(@type_codes, message.type, 0)
      flags_byte = encode_flags_byte(message.flags)
      version_byte = message.version

      header_fixed =
        <<endian_flag, type_byte, flags_byte, version_byte,
          length_and_serial(endianness, body_length, message.serial)::binary>>

      # Combine header parts as iodata
      header_iodata = [header_fixed, header_fields_encoded]

      # Pad header to 8-byte boundary and combine with body
      header_padded = pad_to_8_bytes_iodata(header_iodata)

      {:ok, [header_padded, body_data]}
    end
  end

  @doc """
  Decodes a binary message.

  Parses a D-Bus message from binary format according to the wire format specification.

  ## Parameters

  - `binary` - The binary data to decode

  ## Examples

      iex> message = Rebus.Message.new!(:signal, path: "/", interface: "org.example.Test", member: "Test")
      iex> {:ok, encoded} = Rebus.Message.encode(message)
      iex> {:ok, decoded} = Rebus.Message.decode(IO.iodata_to_binary(encoded))
      iex> decoded.type
      :signal

  ## Returns

  `{:ok, message}` on success, `{:error, reason}` on failure.
  """
  @spec decode(binary()) :: {:ok, t()} | {:error, any()}
  def decode(binary) when is_binary(binary) do
    case decode_frame(binary) do
      {:ok, message} -> {:ok, message}
      {:error, reason, _envelope} -> {:error, reason}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc false
  @spec parse_inbound(binary()) ::
          {:ok, t(), binary()}
          | {:error, :resource_limit,
             %{
               optional(:error_name) => binary(),
               type: message_type(),
               reply_serial: pos_integer() | nil
             }
             | nil, binary()}
          | {:error, any()}
          | nil
  def parse_inbound(binary) when is_binary(binary) do
    case expected_size(binary) do
      {:ok, total_message_size} when byte_size(binary) >= total_message_size ->
        <<message_binary::binary-size(^total_message_size), remaining_data::binary>> = binary

        case decode_frame(message_binary) do
          {:ok, message} ->
            {:ok, message, remaining_data}

          # The frame boundary is known here, so every resource-limit result
          # carries the remainder; the envelope is absent when the limit
          # tripped before the reply fields were validated.
          {:error, :resource_limit, envelope} ->
            {:error, :resource_limit, envelope, remaining_data}

          {:error, :resource_limit} ->
            {:error, :resource_limit, nil, remaining_data}

          {:error, reason} ->
            {:error, reason}
        end

      {:ok, _total_message_size} ->
        nil

      {:error, _reason} = error ->
        error

      nil ->
        nil
    end
  end

  defp decode_frame(binary) when byte_size(binary) > @max_message_size do
    {:error, :message_too_large}
  end

  defp decode_frame(binary) when is_binary(binary) do
    # Parse fixed header
    <<endian_flag, type_byte, flags_byte, version_byte, body_length::binary-size(4),
      serial::binary-size(4), rest::binary>> = binary

    # Determine endianness
    with {:ok, endianness} <- parse_endianness(endian_flag),
         {:ok, type} <- type_from_code(type_byte),
         :ok <- validate_protocol_version(version_byte),
         :ok <- validate_declared_message_size(rest, body_length, endianness) do
      decode_frame_parts(rest, endianness, type, flags_byte, version_byte, body_length, serial)
    end
  rescue
    ResourceLimitError ->
      {:error, :resource_limit}

    # These are the parser failures expected from hostile wire input. Keep this
    # pure protocol module silent; the connection layer reports the safe result.
    _error in @decode_exceptions ->
      {:error, :invalid_message}
  end

  defp decode_frame_parts(rest, endianness, type, flags_byte, version_byte, body_length, serial) do
    body_length = read_uint32(body_length, endianness)
    serial = read_uint32(serial, endianness)
    flags = decode_flags_byte(flags_byte)

    # Decode header fields array - use position-aware decoding for proper struct alignment
    [header_fields_data] = Decoder.decode_at_position("a(yv)", rest, endianness, 12)
    # Parse and validate known header fields.
    {:ok, header_fields} = decode_header_fields(header_fields_data)
    :ok = validate_decoded_header_fields(type, header_fields)

    # An array's declared data length starts after its four-byte length field.
    # It is the authoritative encoded header-fields size and was bounded above.
    {:ok, header_fields_length} = extract_array_length(rest, endianness)
    header_fields_size = 4 + header_fields_length
    header_padded_length = header_padded_length(header_fields_length)

    # The body starts after the padding, relative to the end of the fixed
    # header. Like libdbus, the padding is matched as NUL rather than skipped.
    body_start = header_padded_length - 12
    padding_bits = (body_start - header_fields_size) * 8

    case rest do
      <<_::binary-size(^header_fields_size), 0::size(^padding_bits),
        body_binary::binary-size(^body_length)>> ->
        # Decode body if present
        signature = Map.get(header_fields, :signature, "")

        case decode_body(signature, body_binary, endianness) do
          {:ok, body} ->
            {:ok,
             %__MODULE__{
               type: type,
               flags: flags,
               version: version_byte,
               body_length: body_length,
               serial: serial,
               header_fields: header_fields,
               body: body
             }}

          {:error, :resource_limit} ->
            {:error, :resource_limit, resource_limit_envelope(type, header_fields)}

          {:error, reason} ->
            {:error, reason}
        end

      <<_::binary-size(^body_start), _::binary-size(^body_length)>> ->
        {:error, :invalid_message}

      _ ->
        {:error, :insufficient_data}
    end
  end

  defp resource_limit_envelope(:error, header_fields) do
    %{
      type: :error,
      reply_serial: Map.get(header_fields, :reply_serial),
      error_name: header_fields |> Map.fetch!(:error_name) |> :binary.copy()
    }
  end

  defp resource_limit_envelope(type, header_fields) do
    %{type: type, reply_serial: Map.get(header_fields, :reply_serial)}
  end

  @doc """
  Parses a complete D-Bus message from a binary if sufficient data is available.

  This function checks if the provided binary contains enough data to parse a complete
  D-Bus message (both header and body). If it does, it extracts exactly the right
  amount of data and passes it to `decode/1`. If the binary is too small, returns `nil`.

  This is useful for streaming scenarios where you receive partial data and need to
  determine when you have a complete message.

  ## Parameters

  - `binary` - The binary data that may contain a D-Bus message

  ## Returns

  - `{:ok, message, remaining_data}` - If a complete message was successfully parsed
  - `{:error, reason}` - If the binary contains sufficient data but parsing failed.
    Invalid endianness, message type, and protocol version are rejected as soon
    as the 12-byte fixed header is available. `:message_too_large` is returned
    as soon as the header-fields length is available and the declared complete
    message would exceed `max_message_size/0`.
  - `nil` - If the binary does not contain sufficient data for a complete message

  ## Examples

      # Insufficient data
      iex> Rebus.Message.parse(<<1, 2, 3>>)
      nil

  """
  @spec parse(binary()) :: {:ok, t(), binary()} | {:error, any()} | nil
  def parse(binary) when is_binary(binary) do
    case parse_inbound(binary) do
      {:error, :resource_limit, _envelope, _remaining_data} -> {:error, :resource_limit}
      result -> result
    end
  end

  @doc false
  @spec expected_size(binary()) :: {:ok, pos_integer()} | {:error, atom()} | nil
  def expected_size(binary) when is_binary(binary) and byte_size(binary) >= 12 do
    <<endian_flag, type_byte, _flags_byte, version_byte, body_length::binary-size(4),
      _serial::binary-size(4), rest::binary>> = binary

    with {:ok, endianness} <- parse_endianness(endian_flag),
         {:ok, _type} <- type_from_code(type_byte),
         :ok <- validate_protocol_version(version_byte) do
      body_length = read_uint32(body_length, endianness)

      case declared_message_size(rest, body_length, endianness) do
        {:error, :insufficient_data} -> nil
        result -> result
      end
    end
  end

  def expected_size(_binary), do: nil

  @doc """
  Validates that a message is well-formed according to D-Bus rules.

  Checks that:
  - Message type is valid
  - Required header fields are present for the message type
  - Header field types are correct
  - Message signature is valid

  ## Examples

      iex> message = Rebus.Message.new!(:method_call, path: "/test", member: "Test")
      iex> Rebus.Message.validate(message)
      :ok

  """
  @spec validate(t()) :: :ok | {:error, construction_error()}
  def validate(%__MODULE__{} = message) do
    signature =
      if is_map(message.header_fields),
        do: Map.get(message.header_fields, :signature, ""),
        else: ""

    with :ok <- validate_message_type(message.type),
         :ok <- validate_header_field_types(message.header_fields),
         :ok <- validate_required_fields(message.type, message.header_fields),
         :ok <- validate_signature_format(signature),
         :ok <- validate_body_signature(message.body, signature) do
      UnixFDs.validate_unix_fds(message)
    end
  end

  @doc """
  Gets the message type as an integer code.
  """
  @spec type_code(message_type()) :: non_neg_integer()
  def type_code(type) do
    case Map.get(@type_codes, type) do
      nil -> raise ArgumentError, "Invalid message type: #{inspect(type)}"
      code -> code
    end
  end

  @doc """
  Gets the message type from an integer code.
  """
  @spec type_from_code(non_neg_integer()) ::
          {:ok, message_type()} | {:error, :invalid_message_type}
  def type_from_code(code) do
    case Map.get(@message_types, code) do
      nil -> {:error, :invalid_message_type}
      type -> {:ok, type}
    end
  end

  @doc """
  Gets the signature from the message header fields.

  Returns the signature string if present, or an empty string if not.

  ## Examples

      iex> message = Rebus.Message.new!(:signal, path: "/", interface: "org.example.Test", member: "Test", body: [42], signature: "i")
      iex> Rebus.Message.signature(message)
      "i"

      iex> message = Rebus.Message.new!(:signal, path: "/", interface: "org.example.Test", member: "Test")
      iex> Rebus.Message.signature(message)
      ""
  """
  @spec signature(t()) :: String.t()
  def signature(%__MODULE__{} = message) do
    Map.get(message.header_fields, :signature, "")
  end

  @doc """
  Attaches Unix descriptors received as ancillary data to a decoded message.

  This is the boundary between `h` values on the wire (untrusted indexes) and
  actual process-owned descriptors. The header count, local descriptor bound,
  and every index in the decoded body must agree before a descriptor is made
  visible to an application. `fds` must be closed by the caller if this
  function returns an error.
  """
  @spec attach_unix_fds(t(), [Rebus.UnixFD.t()]) ::
          {:ok, t()} | {:error, :invalid_unix_fds | :unix_fd_limit}
  def attach_unix_fds(%__MODULE__{} = message, fds) when is_list(fds) do
    message = %{message | unix_fds: fds}

    case UnixFDs.validate_unix_fds(message) do
      :ok -> {:ok, message}
      {:error, reason} -> {:error, reason}
    end
  end

  def attach_unix_fds(_message, _fds), do: {:error, :invalid_unix_fds}

  # Private helper functions

  defp validate_flags(flags) when is_list(flags) do
    valid_flags = Map.values(@flags)
    invalid = flags -- valid_flags

    if invalid == [] do
      {:ok, flags}
    else
      {:error, :invalid_flags}
    end
  end

  defp validate_flags(_flags), do: {:error, :invalid_flags}

  defp validate_version(1), do: {:ok, 1}

  defp validate_version(_version), do: {:error, :invalid_version}

  defp validate_body(body) when is_list(body), do: {:ok, body}

  defp validate_body(_body), do: {:error, :invalid_body}

  defp get_or_generate_signature(opts, body) do
    case Keyword.get(opts, :signature) do
      nil -> {:ok, generate_signature(body)}
      signature when is_binary(signature) -> {:ok, signature}
      _signature -> {:error, :invalid_signature}
    end
  end

  defp generate_signature([]), do: ""

  defp generate_signature(body), do: Enum.map_join(body, "", &infer_type/1)

  defp infer_type(value)
       when is_integer(value) and value >= -2_147_483_648 and value <= 2_147_483_647,
       do: "i"

  defp infer_type(value) when is_integer(value), do: "x"
  defp infer_type(value) when is_binary(value), do: "s"
  defp infer_type(value) when is_boolean(value), do: "b"
  defp infer_type(value) when is_float(value), do: "d"
  defp infer_type(value) when value in [:infinity, :negative_infinity, :nan], do: "d"
  defp infer_type(value) when is_list(value), do: "a" <> infer_array_type(value)
  defp infer_type(_), do: "v"

  # Default to string array
  defp infer_array_type([]), do: "s"
  defp infer_array_type([first | _]), do: infer_type(first)

  defp extract_header_fields(opts) do
    field_keys = Map.keys(@field_codes)

    fields =
      for key <- field_keys, Keyword.has_key?(opts, key), into: %{} do
        {key, Keyword.get(opts, key)}
      end

    {:ok, fields}
  end

  defp put_signature_field(header_fields, ""), do: {:ok, header_fields}

  defp put_signature_field(header_fields, signature),
    do: {:ok, Map.put(header_fields, :signature, signature)}

  defp validate_required_fields(type, header_fields) do
    required = Map.get(@required_fields, type, [])
    missing = required -- Map.keys(header_fields)

    if missing == [] do
      :ok
    else
      {:error, {:missing_header_field, hd(missing)}}
    end
  end

  defp validate_header_fields(header_fields) do
    # Validate each header field type
    Enum.reduce_while(header_fields, {:ok, %{}}, fn {field, value}, {:ok, acc} ->
      case validate_header_field(field, value) do
        {:ok, validated_value} -> {:cont, {:ok, Map.put(acc, field, validated_value)}}
        {:error, _} = error -> {:halt, error}
      end
    end)
  end

  defp validate_header_field(:path, path) when is_binary(path),
    do: validated_field(:path, path, WireValue.valid_object_path?(path))

  defp validate_header_field(:interface, interface) when is_binary(interface),
    do: validated_field(:interface, interface, WireValue.valid_interface_name?(interface))

  defp validate_header_field(:member, member) when is_binary(member),
    do: validated_field(:member, member, WireValue.valid_member_name?(member))

  # Error names follow interface naming rules
  defp validate_header_field(:error_name, error_name) when is_binary(error_name),
    do: validated_field(:error_name, error_name, WireValue.valid_error_name?(error_name))

  defp validate_header_field(:destination, dest) when is_binary(dest),
    do: validated_field(:destination, dest, WireValue.valid_bus_name?(dest))

  defp validate_header_field(:sender, sender) when is_binary(sender),
    do: validated_field(:sender, sender, WireValue.valid_bus_name?(sender))

  defp validate_header_field(:signature, signature) when is_binary(signature) do
    case validate_signature_format(signature) do
      :ok -> {:ok, signature}
      {:error, _reason} = error -> error
    end
  end

  # A malformed signature is reported the same way wherever it is supplied,
  # rather than as a generic header-field failure.
  defp validate_header_field(:signature, _signature), do: {:error, :invalid_signature}

  defp validate_header_field(:reply_serial, serial)
       when is_integer(serial) and serial > 0 and serial <= 4_294_967_295,
       do: {:ok, serial}

  defp validate_header_field(:unix_fds, count)
       when is_integer(count) and count >= 0 and count <= 4_294_967_295,
       do: {:ok, count}

  defp validate_header_field(field, _value), do: {:error, {:invalid_header_field, field}}

  defp validated_field(_field, value, true), do: {:ok, value}
  defp validated_field(field, _value, false), do: {:error, {:invalid_header_field, field}}

  defp validate_decoded_header_fields(type, header_fields) do
    with :ok <- validate_required_fields(type, header_fields),
         :ok <- validate_header_field_types(header_fields) do
      :ok
    else
      {:error, :resource_limit} -> raise ResourceLimitError, limit: :nesting
      _ -> raise ArgumentError, "invalid D-Bus header fields"
    end
  end

  defp calculate_body_length(body, signature) do
    with {:ok, body_data} <- encode_body(body, signature, :little) do
      {:ok, IO.iodata_length(body_data)}
    end
  end

  defp encode_body([], "", _endianness), do: {:ok, []}

  defp encode_body(body, signature, endianness) do
    {:ok, Encoder.encode(signature, body, endianness)}
  rescue
    ProtocolLimitError -> {:error, :message_too_large}
    ResourceLimitError -> {:error, :resource_limit}
    _error in @encode_exceptions -> {:error, :invalid_body}
  end

  defp validate_body_signature([], ""), do: :ok
  defp validate_body_signature([], _signature), do: {:error, :invalid_body}

  defp validate_body_signature(body, signature) do
    case encode_body(body, signature, :little) do
      {:ok, _body_data} -> :ok
      {:error, :invalid_body} = error -> error
      {:error, :message_too_large} = error -> error
      {:error, :resource_limit} = error -> error
    end
  end

  defp new_signature(opts) when is_list(opts) do
    case Keyword.fetch(opts, :signature) do
      {:ok, signature} when is_binary(signature) -> signature
      _ -> generate_signature(Keyword.get(opts, :body, []))
    end
  end

  defp encode_header_fields(header_fields, endianness) do
    # Convert header fields to the format expected by encoder:
    # Array of structs where each struct is [field_code, variant]
    fields_data =
      Enum.map(header_fields, fn {field, value} ->
        field_code = Map.fetch!(@field_codes, field)
        field_type = Map.fetch!(@field_types, field)

        # Each struct entry should be a list: [byte_field_code, variant_tuple]
        [field_code, {field_type, value}]
      end)

    {:ok, Encoder.encode_at_position("a(yv)", [fields_data], endianness, 12)}
  rescue
    ProtocolLimitError -> {:error, :message_too_large}
    ResourceLimitError -> {:error, :resource_limit}
    _error in @encode_exceptions -> {:error, :invalid_header_fields}
  end

  defp encode_flags_byte(flags) do
    Enum.reduce(flags, 0, fn flag, acc ->
      case Map.get(@flag_codes, flag) do
        nil -> acc
        code -> bor(acc, code)
      end
    end)
  end

  defp decode_flags_byte(byte) do
    for {code, flag} <- @flags, band(byte, code) != 0 do
      flag
    end
  end

  defp decode_header_fields(fields_data) when is_list(fields_data) do
    Enum.reduce_while(fields_data, {:ok, %{}}, &decode_header_field/2)
  end

  defp decode_header_field([field_code, {field_type, value}], {:ok, acc}) do
    case Map.get(@header_fields, field_code) do
      # Skip unknown field codes
      nil -> {:cont, {:ok, acc}}
      field -> put_decoded_header_field(acc, field, field_type, value)
    end
  end

  defp put_decoded_header_field(acc, field, field_type, value) do
    cond do
      Map.has_key?(acc, field) -> {:halt, {:error, :invalid_message}}
      field_type != Map.fetch!(@field_types, field) -> {:halt, {:error, :invalid_message}}
      true -> {:cont, {:ok, Map.put(acc, field, value)}}
    end
  end

  defp pad_to_8_bytes_iodata(iodata) do
    iodata_size = IO.iodata_length(iodata)
    remainder = rem(iodata_size, 8)

    if remainder == 0 do
      iodata
    else
      padding_size = 8 - remainder
      [iodata, <<0::size(padding_size * 8)>>]
    end
  end

  defp read_uint32(<<value::little-32>>, :little), do: value
  defp read_uint32(<<value::big-32>>, :big), do: value

  defp length_and_serial(:little, body_length, serial),
    do: <<body_length::little-32, serial::little-32>>

  defp length_and_serial(:big, body_length, serial),
    do: <<body_length::big-32, serial::big-32>>

  defp parse_endianness(?l), do: {:ok, :little}
  defp parse_endianness(?B), do: {:ok, :big}
  defp parse_endianness(_), do: {:error, :invalid_endianness}

  defp validate_protocol_version(1), do: :ok
  defp validate_protocol_version(_), do: {:error, :unsupported_protocol_version}

  defp validate_declared_message_size(rest, body_length, endianness) do
    case declared_message_size(rest, read_uint32(body_length, endianness), endianness) do
      {:error, :message_too_large} -> {:error, :message_too_large}
      _ -> :ok
    end
  end

  defp declared_message_size(rest, body_length, endianness) do
    with {:ok, header_fields_length} <- extract_array_length(rest, endianness),
         {:ok, _header_padded_length, total_size} <-
           frame_geometry(header_fields_length, body_length) do
      {:ok, total_size}
    end
  end

  # Bounded padded header length and complete frame size for a header-fields
  # array data length (the array's declared length, excluding its own length
  # field). Callers that are already inside a bounded frame use
  # `header_padded_length/1` instead of repeating the limit checks.
  defp frame_geometry(header_fields_length, _body_length)
       when header_fields_length > @max_array_size,
       do: {:error, :message_too_large}

  defp frame_geometry(header_fields_length, body_length) do
    header_padded_length = header_padded_length(header_fields_length)
    total_size = header_padded_length + body_length

    if total_size <= @max_message_size do
      {:ok, header_padded_length, total_size}
    else
      {:error, :message_too_large}
    end
  end

  # Fixed header (12 bytes) plus the header-fields array and its length field,
  # padded to the body's 8-byte boundary.
  defp header_padded_length(header_fields_length),
    do: div(12 + 4 + header_fields_length + 7, 8) * 8

  # D-Bus arrays start with a 4-byte length field.
  defp extract_array_length(<<array_length::little-32, _rest::binary>>, :little),
    do: {:ok, array_length}

  defp extract_array_length(<<array_length::big-32, _rest::binary>>, :big),
    do: {:ok, array_length}

  defp extract_array_length(_binary, _endianness), do: {:error, :insufficient_data}

  defp validate_message_type(type) when type in @message_type_values, do: :ok
  defp validate_message_type(_type), do: {:error, :invalid_type}

  defp validate_header_field_types(header_fields) when is_map(header_fields) do
    Enum.reduce_while(header_fields, :ok, fn
      {field, value}, :ok when is_map_key(@field_types, field) ->
        case validate_header_field(field, value) do
          {:ok, _validated_value} -> {:cont, :ok}
          {:error, reason} -> {:halt, {:error, reason}}
        end

      {field, _value}, :ok ->
        {:halt, {:error, {:unknown_header_field, field}}}
    end)
  end

  defp validate_header_field_types(_header_fields), do: {:error, :invalid_header_fields}

  defp validate_encodable_header_fields(header_fields) do
    case validate_header_field_types(header_fields) do
      :ok -> :ok
      {:error, :resource_limit} -> {:error, :resource_limit}
      {:error, _reason} -> {:error, :invalid_header_fields}
    end
  end

  defp validate_encodable_envelope(message) do
    with :ok <- validate_message_type(message.type),
         {:ok, _flags} <- validate_flags(message.flags),
         {:ok, _version} <- validate_version(message.version),
         true <-
           is_integer(message.serial) and message.serial > 0 and message.serial <= 4_294_967_295,
         true <- is_map(message.header_fields),
         :ok <- validate_required_fields(message.type, message.header_fields) do
      :ok
    else
      _ -> {:error, :invalid_message}
    end
  end

  defp decode_body("", <<>>, _endianness), do: {:ok, []}
  defp decode_body("", _body_binary, _endianness), do: {:error, :invalid_message}

  defp decode_body(signature, body_binary, endianness) do
    case Decoder.decode_with_position(signature, body_binary, endianness) do
      {body, consumed} when consumed == byte_size(body_binary) -> {:ok, body}
      _ -> {:error, :invalid_message}
    end
  rescue
    ResourceLimitError -> {:error, :resource_limit}
    _error in @decode_exceptions -> {:error, :invalid_message}
  end

  defp validate_signature_format(signature) when is_binary(signature) do
    case Signature.parse(signature) do
      {:ok, _types} -> :ok
      {:error, :resource_limit} -> {:error, :resource_limit}
      {:error, :invalid_signature} -> {:error, :invalid_signature}
    end
  end
end
