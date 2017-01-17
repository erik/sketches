defmodule Layabout.Store do
  def start_link do
    IO.puts "Starting store."
    Agent.start_link(fn -> 0 end, name: __MODULE__)
  end

end
