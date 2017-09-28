defmodule Pronk.ServerSupervisor do
  use Supervisor

  @name Pronk.ServerSupervisor

  def start_link(_opts) do
    Supervisor.start_link(__MODULE__, :ok, name: @name)
  end

  def start_connection do
    Supervisor.start_child(@name, [])
  end

  def init(:ok) do
    Supervisor.init([Pronk.ServerConnection], strategy: :simple_one_for_one)
  end
end
