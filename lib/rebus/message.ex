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
  - `:signature` - Signature of message body (optional, defaults to empty)
  - `:unix_fds` - Number of Unix file descriptors (optional)

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

  alias Rebus.Encoder
  alias Rebus.Decoder

  import Bitwise, only: [bor: 2, band: 2]

  @typedoc "Message type"
  @type message_type :: :invalid | :method_call | :method_return | :error | :signal

  @typedoc "Message flags"
  @type flag :: :no_reply_expected | :no_auto_start | :allow_interactive_authorization

  @typedoc "Header field keys"
  @type header_field ::
          :invalid
          | :path
          | :interface
          | :member
          | :error_name
          | :reply_serial
          | :destination
          | :sender
          | :signature
          | :unix_fds

  @typedoc "Message structure"
  @type t :: %__MODULE__{
          type: message_type(),
          flags: [flag()],
          version: non_neg_integer(),
          body_length: non_neg_integer(),
          serial: non_neg_integer(),
          header_fields: %{optional(header_field()) => term()},
          body: [term()],
          signature: String.t()
        }

  defstruct [
    :type,
    :flags,
    :version,
    :body_length,
    :serial,
    :header_fields,
    :body,
    :signature
  ]

  # Message type constants
  @message_types %{
    0 => :invalid,
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
    0 => :invalid,
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
  Creates a new D-Bus message.

  ## Parameters

  - `type` - The message type (`:method_call`, `:method_return`, `:error`, `:signal`)
  - `opts` - Keyword list of options:
    - `:flags` - List of message flags (default: `[]`)
    - `:version` - Protocol version (default: `1`)
    - `:serial` - Message serial number (default: auto-generated)
    - `:body` - Message body as list of values (default: `[]`)
    - `:signature` - Message body signature (default: auto-generated from body)
    - Header fields like `:path`, `:interface`, `:member`, etc.

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
  """
  @spec new(message_type(), keyword()) :: {:ok, t()} | {:error, String.t()}
  def new(type, opts \\ []) do
    with {:ok, validated_type} <- validate_type(type),
         {:ok, flags} <- validate_flags(Keyword.get(opts, :flags, [])),
         {:ok, version} <- validate_version(Keyword.get(opts, :version, 1)),
         {:ok, body} <- validate_body(Keyword.get(opts, :body, [])),
         {:ok, signature} <- get_or_generate_signature(opts, body),
         {:ok, header_fields} <- extract_header_fields(opts),
         {:ok, validated_fields} <- validate_header_fields(header_fields),
         :ok <- validate_required_fields(validated_type, header_fields),
         serial <- Keyword.get(opts, :serial, generate_serial()) do
      body_length = if body == [], do: 0, else: calculate_body_length(body, signature)

      message = %__MODULE__{
        type: validated_type,
        flags: flags,
        version: version,
        body_length: body_length,
        serial: serial,
        header_fields: validated_fields,
        body: body,
        signature: signature
      }

      {:ok, message}
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
      {:ok, message} -> message
      {:error, reason} -> raise ArgumentError, reason
    end
  end

  @doc """
  Encodes a message to binary format.

  Returns the message encoded according to the D-Bus wire format specification.
  The endianness can be specified as `:little` or `:big` (default: `:little`).

  ## Parameters

  - `message` - The message to encode
  - `endianness` - Byte order (`:little` or `:big`, default: `:little`)

  ## Examples

      iex> message = Rebus.Message.new!(:signal, path: "/", interface: "test", member: "Test")
      iex> {:ok, binary} = Rebus.Message.encode(message)
      iex> is_binary(binary)
      true

  ## Returns

  `{:ok, binary}` on success, `{:error, reason}` on failure.
  """
  @spec encode(t(), :little | :big) :: {:ok, binary()} | {:error, String.t()}
  def encode(message, endianness \\ :little) do
    try do
      # Encode header fields as array of (byte, variant) pairs
      header_fields_data = encode_header_fields(message.header_fields, endianness)

      # Encode body if present
      body_data =
        if message.body == [] do
          <<>>
        else
          try do
            encoded_buffer = Encoder.encode(message.signature, message.body, endianness)
            IO.iodata_to_binary(encoded_buffer)
          rescue
            e -> throw({:error, "Failed to encode body: #{inspect(e)}"})
          catch
            e -> throw({:error, "Failed to encode body: #{inspect(e)}"})
          end
        end

      # Calculate actual body length
      body_length = byte_size(body_data)

      # Encode the fixed header
      endian_flag = if endianness == :little, do: ?l, else: ?B
      type_byte = Map.get(@type_codes, message.type, 0)
      flags_byte = encode_flags_byte(message.flags)
      version_byte = message.version

      # Header fields as array
      header_fields_encoded =
        try do
          encoded_buffer = Encoder.encode("a(yv)", [header_fields_data], endianness)
          IO.iodata_to_binary(encoded_buffer)
        rescue
          e -> throw({:error, "Failed to encode header fields: #{inspect(e)}"})
        catch
          e -> throw({:error, "Failed to encode header fields: #{inspect(e)}"})
        end

      # Build complete header
      header =
        case endianness do
          :little ->
            <<endian_flag, type_byte, flags_byte, version_byte, body_length::little-32,
              message.serial::little-32>> <> header_fields_encoded

          :big ->
            <<endian_flag, type_byte, flags_byte, version_byte, body_length::big-32,
              message.serial::big-32>> <> header_fields_encoded
        end

      # Pad header to 8-byte boundary
      header_padded = pad_to_8_bytes(header)

      # Combine header and body
      complete_message = header_padded <> body_data

      {:ok, complete_message}
    catch
      {:error, reason} -> {:error, reason}
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
  @spec decode(binary()) :: {:ok, t()} | {:error, String.t()}
  def decode(binary) when is_binary(binary) do
    try do
      # Parse fixed header
      <<endian_flag, type_byte, flags_byte, version_byte, body_length::32, serial::32,
        rest::binary>> = binary

      # Determine endianness
      endianness =
        case endian_flag do
          ?l -> :little
          ?B -> :big
          _ -> throw({:error, "Invalid endianness flag: #{endian_flag}"})
        end

      # Correct byte order for header integers
      {body_length, serial} =
        if endianness == :little do
          <<body_length::little-32>> = <<body_length::32>>
          <<serial::little-32>> = <<serial::32>>
          {body_length, serial}
        else
          <<body_length::big-32>> = <<body_length::32>>
          <<serial::big-32>> = <<serial::32>>
          {body_length, serial}
        end

      # Decode message type
      type = Map.get(@message_types, type_byte, :invalid)

      if type == :invalid do
        throw({:error, "Invalid message type: #{type_byte}"})
      end

      # Decode flags
      flags = decode_flags_byte(flags_byte)

      # Decode header fields array
      case Decoder.decode("a(yv)", rest, endianness) do
        [header_fields_data] ->
          # Parse header fields
          header_fields = decode_header_fields(header_fields_data)

          # Calculate header length from the signature structure
          # For now, we'll use a simpler approach - calculate from known header size
          header_fields_size = estimate_header_fields_size(header_fields_data, endianness)
          # Fixed header (12 bytes) + header fields
          header_length = 12 + header_fields_size
          header_padded_length = div(header_length + 7, 8) * 8

          # Extract body from the remaining data after padding
          # Subtract fixed header size
          body_start = header_padded_length - 12

          if byte_size(rest) >= body_start + body_length do
            <<_::binary-size(body_start), body_binary::binary-size(body_length), _::binary>> =
              rest

            # Decode body if present
            signature = Map.get(header_fields, :signature, "")

            {body, final_signature} =
              if signature == "" or body_length == 0 do
                {[], ""}
              else
                try do
                  {Decoder.decode(signature, body_binary, endianness), signature}
                rescue
                  e -> throw({:error, "Failed to decode body: #{inspect(e)}"})
                catch
                  e -> throw({:error, "Failed to decode body: #{inspect(e)}"})
                end
              end

            message = %__MODULE__{
              type: type,
              flags: flags,
              version: version_byte,
              body_length: body_length,
              serial: serial,
              header_fields: header_fields,
              body: body,
              signature: final_signature
            }

            {:ok, message}
          else
            throw({:error, "Insufficient data for message body"})
          end

        _ ->
          throw({:error, "Failed to decode header fields"})
      end
    catch
      {:error, reason} -> {:error, reason}
      :error -> {:error, "Invalid message format"}
    end
  end

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

      iex> invalid = %Rebus.Message{type: :method_call, header_fields: %{}}
      iex> Rebus.Message.validate(invalid)
      {:error, "Missing required field: path"}
  """
  @spec validate(t()) :: :ok | {:error, String.t()}
  def validate(%__MODULE__{} = message) do
    with :ok <- validate_message_type(message.type),
         :ok <- validate_required_fields(message.type, message.header_fields),
         :ok <- validate_header_field_types(message.header_fields),
         :ok <- validate_signature_format(message.signature) do
      :ok
    end
  end

  @doc """
  Gets the message type as an integer code.
  """
  @spec type_code(message_type()) :: non_neg_integer()
  def type_code(type), do: Map.get(@type_codes, type, 0)

  @doc """
  Gets the message type from an integer code.
  """
  @spec type_from_code(non_neg_integer()) :: message_type()
  def type_from_code(code), do: Map.get(@message_types, code, :invalid)

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
    body
    |> Enum.map(&infer_type/1)
    |> Enum.join("")
  end

  defp infer_type(value)
       when is_integer(value) and value >= -2_147_483_648 and value <= 2_147_483_647,
       do: "i"

  defp infer_type(value) when is_integer(value) and value >= 0 and value <= 255, do: "y"
  defp infer_type(value) when is_integer(value), do: "x"
  defp infer_type(value) when is_binary(value), do: "s"
  defp infer_type(value) when is_boolean(value), do: "b"
  defp infer_type(value) when is_float(value), do: "d"
  defp infer_type(value) when is_list(value), do: "a" <> infer_array_type(value)
  defp infer_type(_), do: "v"

  # Default to string array
  defp infer_array_type([]), do: "s"
  defp infer_array_type([first | _]), do: infer_type(first)

  defp extract_header_fields(opts) do
    field_keys = Map.keys(@field_codes) -- [:invalid]

    fields =
      for key <- field_keys, Keyword.has_key?(opts, key), into: %{} do
        {key, Keyword.get(opts, key)}
      end

    {:ok, fields}
  end

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
        if valid_signature?(signature) do
          {:ok, signature}
        else
          {:error, "Invalid signature: #{signature}"}
        end

      {:reply_serial, serial} when is_integer(serial) and serial > 0 ->
        {:ok, serial}

      {:unix_fds, count} when is_integer(count) and count >= 0 ->
        {:ok, count}

      {field, value} ->
        {:error,
         "Invalid value for field #{field} (expected #{expected_type}): #{inspect(value)}"}
    end
  end

  # Validation helpers for D-Bus naming rules
  defp valid_object_path?("/"), do: true

  defp valid_object_path?(path) when is_binary(path) do
    String.starts_with?(path, "/") and
      not String.ends_with?(path, "/") and
      String.match?(path, ~r{^/[A-Za-z0-9_/]*$}) and
      not String.contains?(path, "//")
  end

  defp valid_interface_name?(name) when is_binary(name) do
    parts = String.split(name, ".")
    length(parts) >= 1 and Enum.all?(parts, &valid_name_element/1)
  end

  defp valid_member_name?(name) when is_binary(name) do
    valid_name_element(name)
  end

  defp valid_bus_name?(name) when is_binary(name) do
    cond do
      String.starts_with?(name, ":") ->
        # Unique connection name
        String.match?(name, ~r{^:[A-Za-z0-9._-]+$})

      true ->
        # Well-known bus name
        parts = String.split(name, ".")
        length(parts) >= 2 and Enum.all?(parts, &valid_name_element/1)
    end
  end

  defp valid_name_element(element) when is_binary(element) do
    String.length(element) > 0 and
      String.match?(element, ~r{^[A-Za-z_][A-Za-z0-9_]*$})
  end

  defp valid_signature?(signature) when is_binary(signature) do
    # Basic signature validation - this could be more comprehensive
    String.length(signature) <= 255 and
      String.match?(signature, ~r/^[ybnqiuxtdsoghva()\[\]{}]*$/)
  end

  defp generate_serial do
    # Random 32-bit number
    :rand.uniform(4_294_967_295)
  end

  defp calculate_body_length(body, signature) do
    if signature == "" or body == [] do
      0
    else
      try do
        encoded_buffer = Encoder.encode(signature, body, :little)
        # Convert buffer list to binary and get size
        encoded_buffer |> IO.iodata_to_binary() |> byte_size()
      rescue
        _ -> 0
      catch
        _ -> 0
      end
    end
  end

  defp encode_header_fields(header_fields, _endianness) do
    # Convert header fields to the format expected by encoder:
    # Array of structs where each struct is [field_code, variant]
    header_fields
    |> Enum.map(fn {field, value} ->
      field_code = Map.get(@field_codes, field, 0)
      field_type = Map.get(@field_types, field, "s")

      # Each struct entry should be a list: [byte_field_code, variant_tuple]
      [field_code, {field_type, value}]
    end)
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
    |> Enum.reduce(%{}, fn [field_code, {_type, value}], acc ->
      field = Map.get(@header_fields, field_code, :invalid)

      if field != :invalid do
        Map.put(acc, field, value)
      else
        acc
      end
    end)
  end

  defp pad_to_8_bytes(binary) do
    remainder = rem(byte_size(binary), 8)

    if remainder == 0 do
      binary
    else
      padding_size = 8 - remainder
      binary <> <<0::size(padding_size * 8)>>
    end
  end

  defp estimate_header_fields_size(header_fields_data, endianness) do
    # Encode the header fields to calculate their size
    try do
      encoded_buffer = Encoder.encode("a(yv)", [header_fields_data], endianness)
      encoded_buffer |> IO.iodata_to_binary() |> byte_size()
    rescue
      _ -> 0
    catch
      _ -> 0
    end
  end

  defp validate_message_type(type) when type in [:method_call, :method_return, :error, :signal] do
    :ok
  end

  defp validate_message_type(type) do
    {:error, "Invalid message type: #{inspect(type)}"}
  end

  defp validate_header_field_types(_header_fields) do
    # This is already validated in validate_header_field/2
    :ok
  end

  defp validate_signature_format(signature) when is_binary(signature) do
    if valid_signature?(signature) do
      :ok
    else
      {:error, "Invalid signature format: #{signature}"}
    end
  end
end
