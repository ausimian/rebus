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
  zero-based wire indexes. Outbound descriptors are borrowed and are never
  closed by Rebus. Inbound descriptors appear in `message.unix_fds` only on a
  successfully delivered live call reply and are then owned by that receiving
  process, which must close each one exactly once with `Rebus.UnixFD.close/1`
  or adopt it using a suitable OTP/OS API. Rebus retains inbound descriptors
  until the public `Rebus.call/3` delivery is acknowledged internally, so a
  caller timeout, cancellation, caller death, or connection teardown closes
  them instead. Signals and orphaned replies do not transfer descriptors.

  ## Message Flags

  - `:no_reply_expected` - Don't expect a reply
  - `:no_auto_start` - Don't auto-start destination service
  - `:allow_interactive_authorization` - Allow interactive authorization

  ## Examples

      # Create a method call message
      iex> Rebus.Message.new(:method_call,
      ...>   path: "/com/example/Object",
      ...>   interface: "com.example.Interface",
      ...>   member: "Method",
      ...>   destination: "com.example.Service",
      ...>   body: [42, "hello"],
      ...>   signature: "is"
      ...> )

      # Create a signal message
      iex> Rebus.Message.new(:signal,
      ...>   path: "/com/example/Object",
      ...>   interface: "com.example.Interface",
      ...>   member: "SignalName",
      ...>   body: ["value"],
      ...>   signature: "s"
      ...> )

      # Create an error reply
      iex> Rebus.Message.new(:error,
      ...>   error_name: "com.example.Error.Failed",
      ...>   reply_serial: 123,
      ...>   body: ["Error message"],
      ...>   signature: "s"
      ...> )
  """

  use TypedStruct

  alias Rebus.Decoder
  alias Rebus.Encoder
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
          String.t()
          | :invalid_body
          | :invalid_unix_fds
          | :message_too_large
          | :resource_limit
          | :unix_fd_limit
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
    header_fields_length = header_fields_size - 4

    header_padded_length =
      (12 + header_fields_size)
      |> then(&(div(&1 + 7, 8) * 8))

    if header_fields_length <= @max_array_size and
         header_padded_length + body_length <= @max_message_size do
      :ok
    else
      {:error, :message_too_large}
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

      iex> Rebus.Message.new(:method_call,
      ...>   path: "/com/example/Object",
      ...>   member: "TestMethod"
      ...> )
      %Rebus.Message{type: :method_call, ...}

  ## Errors

  Returns `{:error, reason}` if:
  - Invalid message type
  - Missing required header fields
  - Invalid header field types
  - Invalid signature
  - Body values cannot be encoded by the signature (`:invalid_body`)
  - Encoded message exceeds the D-Bus size limit (`:message_too_large`)
  - Local structural, nesting, or scalar materialization caps are exceeded
    (`:resource_limit`)
  """
  @spec new(message_type(), keyword()) :: {:ok, t()} | {:error, construction_error()}
  def new(type, opts \\ []) do
    with {:ok, validated_type} <- validate_type(type),
         {:ok, flags} <- validate_flags(Keyword.get(opts, :flags, [])),
         {:ok, version} <- validate_version(Keyword.get(opts, :version, 1)),
         {:ok, body} <- validate_body(Keyword.get(opts, :body, [])),
         {:ok, signature} <- get_or_generate_signature(opts, body),
         :ok <- validate_signature_format(signature),
         {:ok, unix_fds} <- extract_unix_fds(opts),
         {:ok, header_fields} <- extract_header_fields(opts),
         {:ok, validated_fields} <- validate_header_fields(header_fields),
         :ok <- validate_required_fields(validated_type, header_fields),
         {:ok, body_length} <- calculate_body_length(body, signature),
         :ok <- validate_unix_fd_indices(signature, body, unix_fds) do
      # Add signature to header_fields if body is present
      validated_fields =
        if signature != "",
          do: Map.put(validated_fields, :signature, signature),
          else: validated_fields

      with {:ok, validated_fields} <- put_unix_fd_count(validated_fields, unix_fds),
           {:ok, header_fields_data} <- encode_header_fields(validated_fields, :little),
           :ok <- validate_encoded_size(IO.iodata_length(header_fields_data), body_length) do
        {:ok,
         %__MODULE__{
           type: validated_type,
           flags: flags,
           version: version,
           body_length: body_length,
           serial: 1,
           header_fields: validated_fields,
           body: body,
           unix_fds: unix_fds
         }}
      end
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

      {:error, :message_too_large} ->
        raise ArgumentError, "message exceeds the D-Bus size limit"

      {:error, :resource_limit} ->
        raise ArgumentError,
              "message exceeds a local resource limit (fixed-width scalar arrays allow at most #{max_scalar_elements()} elements per encode)"

      {:error, :unix_fd_limit} ->
        raise ArgumentError, "message exceeds the Unix file descriptor limit"

      {:error, :invalid_unix_fds} ->
        raise ArgumentError, "Unix file descriptors do not match the message body"

      {:error, reason} when is_binary(reason) ->
        raise ArgumentError, reason
    end
  end

  @doc """
  Encodes a message to iodata format.

  Returns the message encoded according to the D-Bus wire format specification.
  The endianness can be specified as `:little` or `:big` (default: `:little`).

  ## Parameters

  - `message` - The message to encode
  - `endianness` - Byte order (`:little` or `:big`, default: `:little`)

  ## Examples

      iex> message = Rebus.Message.new!(:signal, path: "/", interface: "test", member: "Test")
      iex> {:ok, iodata} = Rebus.Message.encode(message)
      iex> is_binary(IO.iodata_to_binary(iodata))
      true

  ## Returns

  `{:ok, iodata}` on success. Returns `{:error, :invalid_body}` when the
  message body does not match its signature, `{:error, :invalid_header_fields}`
  for invalid header values, or `{:error, :invalid_message}` for an invalid
  fixed header or missing required fields. Returns `{:error, :message_too_large}`
  when the encoded frame exceeds the D-Bus message or header-fields limits, or
  `{:error, :resource_limit}` when a local structural, nesting, or scalar cap
  is exhausted.
  """
  @spec encode(t(), :little | :big) :: {:ok, iodata()} | {:error, encoding_error()}
  def encode(message, endianness \\ :little) when endianness in [:little, :big] do
    with :ok <- validate_encodable_envelope(message),
         :ok <- validate_encodable_header_fields(message.header_fields),
         {:ok, header_fields_encoded} <- encode_header_fields(message.header_fields, endianness),
         {:ok, body_data} <-
           encode_body(message.body, Map.get(message.header_fields, :signature, ""), endianness),
         :ok <- validate_unix_fds(message),
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

      # Build complete header as iodata
      header_fixed =
        case endianness do
          :little ->
            <<endian_flag, type_byte, flags_byte, version_byte, body_length::little-32,
              message.serial::little-32>>

          :big ->
            <<endian_flag, type_byte, flags_byte, version_byte, body_length::big-32,
              message.serial::big-32>>
        end

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

      iex> message = Rebus.Message.new!(:signal, path: "/", interface: "test", member: "Test")
      iex> {:ok, encoded} = Rebus.Message.encode(message)
      iex> {:ok, decoded} = Rebus.Message.decode(encoded)
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
             }, binary()}
          | {:error, any()}
          | nil
  def parse_inbound(binary) when is_binary(binary) do
    case expected_size(binary) do
      {:ok, total_message_size} when byte_size(binary) >= total_message_size ->
        <<message_binary::binary-size(^total_message_size), remaining_data::binary>> = binary

        case decode_frame(message_binary) do
          {:ok, message} ->
            {:ok, message, remaining_data}

          {:error, :resource_limit, envelope} ->
            {:error, :resource_limit, envelope, remaining_data}

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
      # Fixed header (12 bytes) + header fields
      header_length = 12 + header_fields_size
      header_padded_length = div(header_length + 7, 8) * 8

      # Extract body from the remaining data after padding
      # Subtract fixed header size
      body_start = header_padded_length - 12

      if byte_size(rest) == body_start + body_length do
        <<_::binary-size(^body_start), body_binary::binary-size(^body_length), _::binary>> =
          rest

        # Decode body if present
        signature = Map.get(header_fields, :signature, "")

        case decode_body(signature, body_binary, endianness) do
          {:ok, body} ->
            message = %__MODULE__{
              type: type,
              flags: flags,
              version: version_byte,
              body_length: body_length,
              serial: serial,
              header_fields: header_fields,
              body: body
            }

            {:ok, message}

          {:error, :resource_limit} ->
            {:error, :resource_limit, resource_limit_envelope(type, header_fields)}

          {:error, reason} ->
            {:error, reason}
        end
      else
        {:error, :insufficient_data}
      end
    end
  rescue
    ResourceLimitError ->
      {:error, :resource_limit}

    _error in [ArgumentError] ->
      {:error, :invalid_message}

    _error in [CaseClauseError, FunctionClauseError, MatchError] ->
      # These are the parser failures expected from hostile wire input. Keep this
      # pure protocol module silent; the connection layer reports the safe result.
      {:error, :invalid_message}
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
         :ok <- validate_body_signature(message.body, signature),
         :ok <- validate_unix_fds(message) do
      :ok
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

      iex> message = Rebus.Message.new!(:signal, path: "/", interface: "test", member: "Test", body: [42], signature: "i")
      iex> Rebus.Message.signature(message)
      "i"

      iex> message = Rebus.Message.new!(:signal, path: "/", interface: "test", member: "Test")
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

    case validate_unix_fds(message) do
      :ok -> {:ok, message}
      {:error, reason} -> {:error, reason}
    end
  end

  def attach_unix_fds(_message, _fds), do: {:error, :invalid_unix_fds}

  # Private helper functions

  defp validate_type(type) when type in [:method_call, :method_return, :error, :signal] do
    {:ok, type}
  end

  defp validate_type(type) do
    {:error, "Invalid message type: #{inspect(type)}"}
  end

  defp validate_flags(flags) when is_list(flags) do
    valid_flags = Map.values(@flags)
    invalid = flags -- valid_flags

    if invalid == [] do
      {:ok, flags}
    else
      {:error, "Invalid flags: #{inspect(invalid)}"}
    end
  end

  defp validate_flags(flags) do
    {:error, "Flags must be a list, got: #{inspect(flags)}"}
  end

  defp validate_version(1), do: {:ok, 1}

  defp validate_version(version) do
    {:error, "Unsupported protocol version: #{version}"}
  end

  defp validate_body(body) when is_list(body), do: {:ok, body}

  defp validate_body(body) do
    {:error, "Body must be a list, got: #{inspect(body)}"}
  end

  defp get_or_generate_signature(opts, body) do
    case Keyword.get(opts, :signature) do
      nil -> {:ok, generate_signature(body)}
      signature when is_binary(signature) -> {:ok, signature}
      signature -> {:error, "Signature must be a string, got: #{inspect(signature)}"}
    end
  end

  defp generate_signature([]), do: ""

  defp generate_signature(body) do
    # This is a simple signature generation - in practice you'd want more sophisticated logic
    Enum.map_join(body, "", &infer_type/1)
  end

  defp infer_type(value)
       when is_integer(value) and value >= -2_147_483_648 and value <= 2_147_483_647,
       do: "i"

  defp infer_type(value) when is_integer(value) and value >= 0 and value <= 255, do: "y"
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

  defp extract_unix_fds(opts) do
    case Keyword.get(opts, :fds, []) do
      fds when is_list(fds) -> validate_unix_fd_list(fds)
      _fds -> {:error, :invalid_unix_fds}
    end
  end

  defp put_unix_fd_count(header_fields, fds) do
    count = length(fds)

    case Map.fetch(header_fields, :unix_fds) do
      :error when count == 0 -> {:ok, header_fields}
      :error -> {:ok, Map.put(header_fields, :unix_fds, count)}
      {:ok, ^count} -> {:ok, header_fields}
      {:ok, _count} -> {:error, :invalid_unix_fds}
    end
  end

  defp validate_unix_fds(%__MODULE__{header_fields: header_fields, unix_fds: fds} = message)
       when is_map(header_fields) and is_list(fds) do
    with {:ok, fds} <- validate_unix_fd_list(fds),
         count <- length(fds),
         ^count <- Map.get(header_fields, :unix_fds, 0),
         :ok <- validate_unix_fd_indices(signature(message), message.body, fds) do
      :ok
    else
      _ -> {:error, :invalid_unix_fds}
    end
  end

  defp validate_unix_fds(_message), do: {:error, :invalid_unix_fds}

  defp validate_unix_fd_list(fds) when length(fds) <= @max_unix_fds do
    if Enum.all?(fds, &(is_integer(&1) and &1 >= 0)) do
      {:ok, fds}
    else
      {:error, :invalid_unix_fds}
    end
  end

  defp validate_unix_fd_list(fds) when is_list(fds), do: {:error, :unix_fd_limit}

  defp validate_unix_fd_indices(signature, body, fds)
       when is_binary(signature) and is_list(body) do
    with {:ok, types} <- Signature.parse(signature),
         :ok <- validate_unix_fd_values(types, body, length(fds)) do
      :ok
    else
      _ -> {:error, :invalid_unix_fds}
    end
  end

  defp validate_unix_fd_indices(_signature, _body, _fds), do: {:error, :invalid_unix_fds}

  defp validate_unix_fd_values([], [], _fd_count), do: :ok

  defp validate_unix_fd_values([type | types], [value | values], fd_count) do
    with :ok <- validate_unix_fd_value(type, value, fd_count) do
      validate_unix_fd_values(types, values, fd_count)
    end
  end

  defp validate_unix_fd_values(_types, _values, _fd_count), do: {:error, :invalid_unix_fds}

  defp validate_unix_fd_value({:unix_fd, _}, index, fd_count)
       when is_integer(index) and index >= 0 and index < fd_count,
       do: :ok

  defp validate_unix_fd_value({:unix_fd, _}, _index, _fd_count), do: {:error, :invalid_unix_fds}

  defp validate_unix_fd_value({:array, type}, values, fd_count) when is_list(values) do
    Enum.reduce_while(values, :ok, fn value, :ok ->
      case validate_unix_fd_value(type, value, fd_count) do
        :ok -> {:cont, :ok}
        error -> {:halt, error}
      end
    end)
  end

  defp validate_unix_fd_value({:struct, types}, values, fd_count) when is_list(values),
    do: validate_unix_fd_values(types, values, fd_count)

  defp validate_unix_fd_value({:dict_entry, key_type, value_type}, {key, value}, fd_count) do
    with :ok <- validate_unix_fd_value(key_type, key, fd_count) do
      validate_unix_fd_value(value_type, value, fd_count)
    end
  end

  defp validate_unix_fd_value({:variant, _}, {nested_signature, value}, fd_count)
       when is_binary(nested_signature) do
    with {:ok, [type]} <- Signature.parse(nested_signature) do
      validate_unix_fd_value(type, value, fd_count)
    else
      _ -> {:error, :invalid_unix_fds}
    end
  end

  defp validate_unix_fd_value({kind, _}, _value, _fd_count)
       when kind in [
              :byte,
              :boolean,
              :int16,
              :uint16,
              :int32,
              :uint32,
              :int64,
              :uint64,
              :double,
              :string,
              :object_path,
              :signature
            ],
       do: :ok

  defp validate_unix_fd_value(_type, _value, _fd_count), do: {:error, :invalid_unix_fds}

  defp validate_required_fields(type, header_fields) do
    required = Map.get(@required_fields, type, [])
    missing = required -- Map.keys(header_fields)

    if missing == [] do
      :ok
    else
      {:error, "Missing required field: #{hd(missing)}"}
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

  defp validate_header_field(field, value) do
    expected_type = Map.get(@field_types, field)

    case {field, value} do
      {:path, path} when is_binary(path) ->
        if valid_object_path?(path) do
          {:ok, path}
        else
          {:error, "Invalid object path: #{path}"}
        end

      {:interface, interface} when is_binary(interface) ->
        if valid_interface_name?(interface) do
          {:ok, interface}
        else
          {:error, "Invalid interface name: #{interface}"}
        end

      {:member, member} when is_binary(member) ->
        if valid_member_name?(member) do
          {:ok, member}
        else
          {:error, "Invalid member name: #{member}"}
        end

      {:error_name, error_name} when is_binary(error_name) ->
        # Error names follow interface naming rules
        if valid_interface_name?(error_name) do
          {:ok, error_name}
        else
          {:error, "Invalid error name: #{error_name}"}
        end

      {:destination, dest} when is_binary(dest) ->
        if valid_bus_name?(dest) do
          {:ok, dest}
        else
          {:error, "Invalid destination: #{dest}"}
        end

      {:sender, sender} when is_binary(sender) ->
        if valid_bus_name?(sender) do
          {:ok, sender}
        else
          {:error, "Invalid sender: #{sender}"}
        end

      {:signature, signature} when is_binary(signature) ->
        case Signature.parse(signature) do
          {:ok, _types} -> {:ok, signature}
          {:error, :resource_limit} -> {:error, :resource_limit}
          {:error, :invalid_signature} -> {:error, "Invalid signature format: #{signature}"}
        end

      {:reply_serial, serial}
      when is_integer(serial) and serial > 0 and serial <= 4_294_967_295 ->
        {:ok, serial}

      {:unix_fds, count} when is_integer(count) and count >= 0 and count <= 4_294_967_295 ->
        {:ok, count}

      {field, value} ->
        {:error,
         "Invalid value for field #{field} (expected #{expected_type}): #{inspect(value)}"}
    end
  end

  # Validation helpers for D-Bus naming rules
  defp valid_object_path?(path), do: WireValue.valid_object_path?(path)

  defp valid_interface_name?(name) when is_binary(name) do
    if WireValue.valid_string?(name) do
      parts = String.split(name, ".")
      parts != [] and Enum.all?(parts, &valid_name_element/1)
    else
      false
    end
  end

  defp valid_member_name?(name) when is_binary(name) do
    WireValue.valid_string?(name) and valid_name_element(name)
  end

  defp valid_bus_name?(name) when is_binary(name) do
    cond do
      not WireValue.valid_string?(name) ->
        false

      String.starts_with?(name, ":") ->
        # Unique connection name
        String.match?(name, ~r{\A:[A-Za-z0-9._-]+\z})

      true ->
        # Well-known bus name
        parts = String.split(name, ".")
        length(parts) >= 2 and Enum.all?(parts, &valid_bus_name_element/1)
    end
  end

  defp valid_bus_name_element(element) when is_binary(element) do
    String.match?(element, ~r/\A[A-Za-z_-][A-Za-z0-9_-]*\z/)
  end

  defp valid_name_element(element) when is_binary(element) do
    String.length(element) > 0 and
      String.match?(element, ~r{\A[A-Za-z_][A-Za-z0-9_]*\z})
  end

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
    ResourceLimitError ->
      {:error, :resource_limit}

    ArgumentError ->
      {:error, :invalid_body}

    CaseClauseError ->
      {:error, :invalid_body}

    FunctionClauseError ->
      {:error, :invalid_body}

    KeyError ->
      {:error, :invalid_body}

    MatchError ->
      {:error, :invalid_body}
  end

  defp validate_body_signature([], ""), do: :ok
  defp validate_body_signature([], _signature), do: {:error, :invalid_body}

  defp validate_body_signature(body, signature) do
    case encode_body(body, signature, :little) do
      {:ok, _body_data} -> :ok
      {:error, :invalid_body} = error -> error
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
    ResourceLimitError -> {:error, :resource_limit}
    ArgumentError -> {:error, :invalid_header_fields}
    CaseClauseError -> {:error, :invalid_header_fields}
    FunctionClauseError -> {:error, :invalid_header_fields}
    KeyError -> {:error, :invalid_header_fields}
    MatchError -> {:error, :invalid_header_fields}
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
    fields_data
    |> Enum.reduce_while({:ok, %{}}, fn [field_code, {field_type, value}], {:ok, acc} ->
      case Map.get(@header_fields, field_code) do
        # Skip unknown field codes
        nil ->
          {:cont, {:ok, acc}}

        field ->
          if Map.has_key?(acc, field) do
            {:halt, {:error, :invalid_message}}
          else
            if field_type == Map.fetch!(@field_types, field) do
              {:cont, {:ok, Map.put(acc, field, value)}}
            else
              {:halt, {:error, :invalid_message}}
            end
          end
      end
    end)
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
    with {:ok, header_fields_length} <- extract_array_length(rest, endianness) do
      if header_fields_length <= @max_array_size do
        header_padded_length =
          (12 + 4 + header_fields_length)
          |> then(&(div(&1 + 7, 8) * 8))

        total_message_size = header_padded_length + body_length

        if total_message_size <= @max_message_size do
          {:ok, total_message_size}
        else
          {:error, :message_too_large}
        end
      else
        {:error, :message_too_large}
      end
    end
  end

  defp extract_array_length(binary, endianness) do
    # D-Bus arrays start with a 4-byte length field
    if byte_size(binary) >= 4 do
      case endianness do
        :little ->
          <<array_length::little-32, _rest::binary>> = binary
          {:ok, array_length}

        :big ->
          <<array_length::big-32, _rest::binary>> = binary
          {:ok, array_length}
      end
    else
      {:error, :insufficient_data}
    end
  end

  defp validate_message_type(type) when type in [:method_call, :method_return, :error, :signal] do
    :ok
  end

  defp validate_message_type(type) do
    {:error, "Invalid message type: #{inspect(type)}"}
  end

  defp validate_header_field_types(header_fields) when is_map(header_fields) do
    Enum.reduce_while(header_fields, :ok, fn
      {field, value}, :ok when is_map_key(@field_types, field) ->
        case validate_header_field(field, value) do
          {:ok, _validated_value} -> {:cont, :ok}
          {:error, reason} -> {:halt, {:error, reason}}
        end

      {_field, _value}, :ok ->
        {:halt, {:error, "Invalid header field"}}
    end)
  end

  defp validate_header_field_types(_header_fields), do: {:error, "Invalid header fields"}

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
    ResourceLimitError ->
      {:error, :resource_limit}

    ArgumentError ->
      {:error, :invalid_message}

    _error in [CaseClauseError, FunctionClauseError, MatchError] ->
      {:error, :invalid_message}
  end

  defp validate_signature_format(signature) when is_binary(signature) do
    case Signature.parse(signature) do
      {:ok, _types} -> :ok
      {:error, :resource_limit} -> {:error, :resource_limit}
      {:error, :invalid_signature} -> {:error, "Invalid signature format: #{signature}"}
    end
  end
end
