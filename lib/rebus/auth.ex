defmodule Rebus.Auth do
  @moduledoc false

  import Bitwise, only: [band: 2]

  @max_mechanisms 64
  @max_mechanism_size 64
  @max_cookie_context_size 128
  @max_cookie_challenge_size 512
  @max_cookie_file_size 65_536
  @max_cookie_line_size 1_024
  @max_cookie_lines 256
  @max_cookie_size 1_024
  @max_home_links 8

  @type auth_error :: :auth_cookie_unavailable | :auth_failed

  @doc false
  @spec parse_rejected(binary()) :: {:ok, [binary()]} | {:error, :auth_failed}
  def parse_rejected("REJECTED"), do: {:ok, []}

  def parse_rejected("REJECTED " <> advertised) do
    mechanisms = :binary.split(advertised, " ", [:global])

    if length(mechanisms) in 1..@max_mechanisms and
         Enum.all?(mechanisms, &valid_mechanism?/1) do
      {:ok, Enum.map(mechanisms, &:binary.copy/1)}
    else
      {:error, :auth_failed}
    end
  end

  def parse_rejected(_line), do: {:error, :auth_failed}

  @doc false
  @spec cookie_response(binary(), non_neg_integer(), binary()) ::
          {:ok, binary()} | {:error, auth_error()}
  def cookie_response(username, uid, encoded_challenge)
      when is_binary(username) and is_integer(uid) and uid >= 0 and
             is_binary(encoded_challenge) do
    with :ok <- validate_username(username),
         {:ok, context, cookie_id, server_challenge} <- parse_cookie_challenge(encoded_challenge),
         {:ok, cookie} <- read_cookie(context, cookie_id, uid) do
      client_challenge = :crypto.strong_rand_bytes(16) |> Base.encode16(case: :lower)

      digest =
        :crypto.hash(:sha, [server_challenge, ":", client_challenge, ":", cookie])
        |> Base.encode16(case: :lower)

      response = Base.encode16(client_challenge <> " " <> digest, case: :lower)
      {:ok, response}
    else
      {:error, reason} -> {:error, reason}
      _ -> {:error, :auth_failed}
    end
  end

  def cookie_response(_username, _uid, _encoded_challenge), do: {:error, :auth_cookie_unavailable}

  defp parse_cookie_challenge(encoded)
       when byte_size(encoded) > 0 and byte_size(encoded) <= @max_cookie_challenge_size * 2 and
              rem(byte_size(encoded), 2) == 0 do
    with {:ok, decoded} <- Base.decode16(encoded, case: :mixed),
         [context, cookie_id, server_challenge] <- :binary.split(decoded, " ", [:global]),
         :ok <- validate_context(context),
         :ok <- validate_cookie_id(cookie_id),
         :ok <- validate_challenge(server_challenge) do
      {:ok, context, cookie_id, server_challenge}
    else
      _ -> {:error, :auth_failed}
    end
  end

  defp parse_cookie_challenge(_encoded), do: {:error, :auth_failed}

  defp read_cookie(context, cookie_id, uid) do
    with {:ok, keyring_dir} <- secure_keyring_dir(uid),
         path <- Path.join(keyring_dir, context),
         {:ok, before} <- secure_lstat(path, :regular, uid),
         {:ok, contents} <- read_bounded_file(path),
         {:ok, after_stat} <- secure_lstat(path, :regular, uid),
         true <- before.size == after_stat.size and after_stat.size == byte_size(contents),
         {:ok, cookie} <- find_cookie(contents, cookie_id) do
      {:ok, cookie}
    else
      _ -> {:error, :auth_cookie_unavailable}
    end
  end

  defp secure_keyring_dir(uid) do
    with home when is_binary(home) and home != <<>> <- user_home(),
         :absolute <- Path.type(home),
         {:ok, resolved_home} <- resolve_home(home, uid),
         path <- Path.join(resolved_home, ".dbus-keyrings"),
         {:ok, _stat} <- secure_lstat(path, :directory, uid) do
      {:ok, path}
    else
      _ -> {:error, :auth_cookie_unavailable}
    end
  end

  # A user may intentionally expose their home through a final symlink (for
  # example, when the real home is on an encrypted volume, behind an
  # automounter, or under a generated profile). That final component may be a
  # chain of up to `@max_home_links` symlinks, and we follow the chain
  # ourselves: each hop has its trailing separators and `.` components
  # normalised before it is `lstat`ed, so a home written as `~/link/` or
  # `~/link/.` cannot slip past resolution; a hop that ends in `..` is rejected,
  # since only following the component before it could reach that parent; and
  # a relative link target is joined onto the link's parent rather than
  # lexically expanded, so `.` and `..` components are resolved by the kernel
  # at the next `lstat` exactly as they are for any path. The directory the
  # chain finally reaches is the one the owner and private-mode checks apply
  # to, and the keyring directory and cookie file are derived from it and
  # lstat-checked, so neither may be a symlink. Intermediate components of the
  # home path are resolved by the operating system, as they are for any path.
  defp resolve_home(home, uid) do
    with {:ok, resolved, stat} <- resolve_home_dir(home),
         true <- is_integer(stat.uid) and stat.uid == uid,
         true <- is_integer(stat.mode) and band(stat.mode, 0o022) == 0 do
      {:ok, resolved}
    else
      _ -> {:error, :auth_cookie_unavailable}
    end
  end

  defp resolve_home_dir(home) do
    with {:ok, home} <- normalise_home(home), do: resolve_home_dir(home, @max_home_links)
  end

  # `hops` counts the links still available to follow, so a chain of exactly
  # `@max_home_links` links resolves - the last of them lands on a directory
  # with `hops` down to zero - and one more link is rejected.
  defp resolve_home_dir(home, hops) do
    case File.lstat(home) do
      {:ok, %File.Stat{type: :directory} = stat} ->
        {:ok, home, stat}

      {:ok, %File.Stat{type: :symlink}} when hops > 0 ->
        follow_home_link(home, hops)

      _ ->
        {:error, :auth_cookie_unavailable}
    end
  end

  defp follow_home_link(home, hops) do
    with {:ok, target} <- File.read_link(home),
         {:ok, resolved} <- expand_home_target(target, home),
         {:ok, resolved} <- normalise_home(resolved) do
      resolve_home_dir(resolved, hops - 1)
    else
      _ -> {:error, :auth_cookie_unavailable}
    end
  end

  # `Path.join/2` rather than `Path.expand/2`: expansion collapses `.` and `..`
  # lexically, in the base as well as in the target, which names a different
  # directory than the kernel reaches whenever an earlier component is itself a
  # symlink. Joining leaves those components in the string for the next `lstat`
  # to resolve physically.
  defp expand_home_target(target, home) do
    case Path.type(target) do
      :absolute -> {:ok, target}
      _ -> {:ok, Path.join(Path.dirname(home), target)}
    end
  end

  # POSIX `lstat` follows a trailing separator and a trailing `.` component, so
  # `lstat("~/link/")` and `lstat("~/link/.")` both report the target's type
  # and would hide the link entirely. Strip them until the last component is a
  # plain name. A final `..` cannot be stripped - it names the parent of the
  # component before it, which `lstat` would have to follow to get there - so
  # the string checked would not be the string used; reject it instead.
  defp normalise_home(home) do
    case strip_trailing_dots(home) do
      "" ->
        {:ok, "/"}

      normalised ->
        if Path.basename(normalised) == "..",
          do: {:error, :auth_cookie_unavailable},
          else: {:ok, normalised}
    end
  end

  defp strip_trailing_dots(home) do
    stripped = String.replace_trailing(home, "/", "")

    if String.ends_with?(stripped, "/."),
      do: strip_trailing_dots(binary_part(stripped, 0, byte_size(stripped) - 2)),
      else: stripped
  end

  defp user_home do
    case System.get_env("HOME") do
      home when is_binary(home) and home != <<>> -> home
      _ -> System.user_home()
    end
  end

  # `lstat` deliberately rejects symlinks. OTP does not expose O_NOFOLLOW for
  # portable raw file opens, so we validate both sides of the bounded read too.
  # Platforms without POSIX owner/mode metadata fail closed rather than guessing.
  defp secure_lstat(path, type, uid) do
    with {:ok, stat} <- File.lstat(path),
         true <- stat.type == type,
         true <- is_integer(stat.uid) and stat.uid == uid,
         true <- is_integer(stat.mode) and band(stat.mode, 0o077) == 0,
         true <- safe_stat_size?(stat, type) do
      {:ok, stat}
    else
      _ -> {:error, :auth_cookie_unavailable}
    end
  end

  defp safe_stat_size?(stat, :regular),
    do: is_integer(stat.size) and stat.size >= 0 and stat.size <= @max_cookie_file_size

  defp safe_stat_size?(_stat, :directory), do: true

  defp read_bounded_file(path) do
    with {:ok, file} <- :file.open(String.to_charlist(path), [:read, :binary, :raw]) do
      result =
        case :file.read(file, @max_cookie_file_size + 1) do
          {:ok, contents} when byte_size(contents) <= @max_cookie_file_size -> {:ok, contents}
          _ -> {:error, :auth_cookie_unavailable}
        end

      :ok = :file.close(file)
      result
    end
  rescue
    _exception -> {:error, :auth_cookie_unavailable}
  catch
    _kind, _reason -> {:error, :auth_cookie_unavailable}
  end

  defp find_cookie(contents, wanted_id) do
    lines = keyring_lines(contents)

    if length(lines) <= @max_cookie_lines do
      lines
      |> Enum.reduce_while({:ok, nil}, &scan_cookie_line(&1, &2, wanted_id))
      |> found_cookie()
    else
      {:error, :auth_cookie_unavailable}
    end
  end

  defp scan_cookie_line(line, {:ok, found}, wanted_id) do
    case parse_cookie_line(line) do
      {:ok, {^wanted_id, _timestamp, cookie}} when is_nil(found) ->
        {:cont, {:ok, cookie}}

      {:ok, {^wanted_id, _timestamp, _cookie}} ->
        {:halt, {:error, :auth_cookie_unavailable}}

      {:ok, _other} ->
        {:cont, {:ok, found}}

      :empty ->
        {:cont, {:ok, found}}

      :error ->
        skip_unparsable_cookie_line(line, wanted_id, found)
    end
  end

  # Cookie files are shared per context and may contain unrelated stale or
  # malformed records. Ignore those bounded lines, while preserving
  # fail-closed handling for a malformed target record.
  defp skip_unparsable_cookie_line(line, wanted_id, found) do
    if malformed_target_cookie_line?(line, wanted_id) do
      {:halt, {:error, :auth_cookie_unavailable}}
    else
      {:cont, {:ok, found}}
    end
  end

  defp found_cookie({:ok, cookie}) when is_binary(cookie), do: {:ok, cookie}
  defp found_cookie(_result), do: {:error, :auth_cookie_unavailable}

  defp keyring_lines(contents) do
    lines = :binary.split(contents, "\n", [:global])

    if List.last(lines) == <<>>, do: Enum.drop(lines, -1), else: lines
  end

  defp malformed_target_cookie_line?(line, wanted_id) do
    case :binary.match(line, " ") do
      {field_size, 1} -> binary_part(line, 0, field_size) == wanted_id
      :nomatch -> line == wanted_id
    end
  end

  defp parse_cookie_line(<<>>), do: :empty

  defp parse_cookie_line(line) when byte_size(line) <= @max_cookie_line_size do
    case :binary.split(line, " ", [:global]) do
      [id, timestamp, cookie] ->
        with :ok <- validate_cookie_id(id),
             :ok <- validate_cookie_id(timestamp),
             :ok <- validate_cookie(cookie) do
          {:ok, {id, timestamp, cookie}}
        else
          _ -> :error
        end

      _ ->
        :error
    end
  end

  defp parse_cookie_line(_line), do: :error

  defp valid_mechanism?(mechanism)
       when byte_size(mechanism) in 1..@max_mechanism_size,
       do: all_bytes?(mechanism, &valid_mechanism_byte?/1)

  defp valid_mechanism?(_mechanism), do: false

  defp valid_mechanism_byte?(byte)
       when byte in ?A..?Z or byte in ?0..?9 or byte in [?_, ?-],
       do: true

  defp valid_mechanism_byte?(_byte), do: false

  defp validate_context(context)
       when byte_size(context) in 1..@max_cookie_context_size do
    if all_bytes?(context, &valid_context_byte?/1), do: :ok, else: :error
  end

  defp validate_context(_context), do: :error

  defp valid_context_byte?(byte) when byte in 0x21..0x7E and byte not in [?/, ?\\, ?.], do: true
  defp valid_context_byte?(_byte), do: false

  defp validate_cookie_id(value) when byte_size(value) in 1..20 do
    if all_bytes?(value, &digit_byte?/1), do: :ok, else: :error
  end

  defp validate_cookie_id(_value), do: :error

  defp validate_challenge(challenge)
       when byte_size(challenge) in 1..@max_cookie_challenge_size do
    if all_bytes?(challenge, &visible_ascii_byte?/1), do: :ok, else: :error
  end

  defp validate_challenge(_challenge), do: :error

  defp validate_cookie(cookie)
       when byte_size(cookie) in 2..@max_cookie_size and rem(byte_size(cookie), 2) == 0 do
    case Base.decode16(cookie, case: :mixed) do
      {:ok, _decoded} -> :ok
      :error -> :error
    end
  end

  defp validate_cookie(_cookie), do: :error

  defp validate_username(username) when byte_size(username) in 1..64 do
    if all_bytes?(username, &visible_ascii_byte?/1), do: :ok, else: :error
  end

  defp validate_username(_username), do: :error

  defp digit_byte?(byte), do: byte in ?0..?9

  defp visible_ascii_byte?(byte), do: byte in 0x21..0x7E

  # Walk the binary directly: no intermediate list, and the first byte that
  # fails the predicate ends the walk. Every caller guards a non-empty binary;
  # the empty binary trivially satisfies the predicate.
  defp all_bytes?(<<>>, _predicate), do: true

  defp all_bytes?(<<byte, rest::binary>>, predicate),
    do: predicate.(byte) and all_bytes?(rest, predicate)
end
