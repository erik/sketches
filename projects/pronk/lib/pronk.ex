defmodule Pronk do
  require Logger
  use Application

  def start(_type, _args) do
    import Supervisor.Spec

    children = [
      Pronk.ServerSupervisor,
      supervisor(Task.Supervisor, [[name: Pronk.TaskSupervisor]]),
      worker(Task, [Pronk.ClientConnection, :listen, [9999]]),
      worker(Pronk.Registry, []),
    ]

    Logger.info "pronk pronk pronk"

    opts = [strategy: :one_for_one, name: Pronk.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
