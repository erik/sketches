defmodule Mix.Tasks.Pronk.Config do
  use Mix.Task
  require Logger

  defp prompt(text) do
    IO.gets("#{text}: ")
  end

  defp prompt_number(text) do
    case prompt(text) |> Integer.parse do
      :error ->
        IO.puts("Invalid integer")
        prompt_number(text)
      number ->
        number
    end
  end

  defp confirm(text) do
    case prompt(text <> " [y/n]") |> String.downcase |> String.trim do
      resp when resp in ["y", "yes"] ->
        true

      resp when resp in ["n", "no"] ->
        false

      _ ->
        IO.puts("yes or no")
        confirm(text)
    end
  end

  def run(_args) do
    config_keys = [
      [:bind_host, :string, "Bind host"],
      [:port,      :number, "Port to listen on."],
    ]

    IO.puts "Eventually this will create a config file."

    if confirm("Sound good?") do
      IO.puts "Yeah"
    end

  end
end
