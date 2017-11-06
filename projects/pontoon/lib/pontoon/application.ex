defmodule Pontoon.Application do
  require Logger
  use Application

  def start(_type, _args) do
    import Supervisor.Spec

    children = [
      worker(Pontoon.Broadcast, []),
      worker(Pontoon.Membership, [])
    ]

    Logger.info("Initializing...")

    opts = [strategy: :one_for_one, name: Pontoon.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
