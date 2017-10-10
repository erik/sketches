defmodule Pronk.IRC do
  def parse(message) when length(message) == 0, do: {:ok, nil}

  # message    =  [ ":" prefix SPACE ] command [ params ] crlf
  # prefix     =  servername / ( nickname [ [ "!" user ] "@" host ] )
  # command    =  1*letter / 3digit
  # params     =  *14( SPACE middle ) [ SPACE ":" trailing ]
  # =/ 14( SPACE middle ) [ SPACE [ ":" ] trailing ]

  # nospcrlfcl =  %x01-09 / %x0B-0C / %x0E-1F / %x21-39 / %x3B-FF
  # ; any octet except NUL, CR, LF, " " and ":"
  # middle     =  nospcrlfcl *( ":" / nospcrlfcl )
  # trailing   =  *( ":" / " " / nospcrlfcl )

  # SPACE      =  %x20        ; space character
  # crlf       =  %x0D %x0A   ; "carriage return" "linefeed"

  def parse(message) do
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

    [first, last] =
      case message |> String.trim |> String.split(" :", parts: 2, trim: true) do
        [hd] -> [hd, []]
        [hd, tl] -> [hd, [tl]]
      end

    case first |> String.split(" ") do
      [":" <> prefix, command | params] ->
        [tags, prefix, command, params ++ last]

      [command | params] ->
        [tags, command, params ++ last]
    end
  end
end
