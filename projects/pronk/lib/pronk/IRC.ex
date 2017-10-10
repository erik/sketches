defmodule Pronk.IRC do
  defmodule Line do
    defstruct [:tags, :command, :prefix, :params]
  end

  # message = ["@" tags ] [ ":" prefix SPACE ] command [ params ] crlf
  # prefix  = servername / ( nickname [ [ "!" user ] "@" host ] )
  def parse(message) when message == "", do: nil
  def parse(message) do
    # Parse out IRCv3 tags
    [tags, message] =
      case message |> String.split(" ", parts: 2, trim: true) do
        ["@" <> tags, message] ->
          tag_map = tags
          |> String.split(";")
          |> Enum.map(fn tag -> tag |> String.split("=", parts: 2) end)
          |> Enum.into(%{}, fn [k, v] -> {k, v} end)

          [tag_map, message]

        _ ->
          [Map.new, message]
      end

    # Last param could contain whitespace, and starts with ':'
    [first, final] =
      case message |> String.trim |> String.split(" :", parts: 2, trim: true) do
        [hd] -> [hd, []]
        [hd, tl] -> [hd, [tl]]
      end

    case first |> String.split(" ") do
      [":" <> prefix, command | params] ->
        %Line{tags: tags, prefix: prefix, command: command, params: params ++ final}

      [command | params] ->
        %Line{tags: tags, prefix: nil, command: command, params: params ++ final}
    end
  end
end
