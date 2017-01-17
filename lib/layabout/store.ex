defmodule Layabout.Store do
  def start_link do
    IO.puts "Starting store."
    Agent.start_link(fn -> %{} end, name: __MODULE__)
  end

  def log_active(_user) do
    Agent.update fn state ->
      state
    end
  end

  def log_inactive(_user) do
  end

end
