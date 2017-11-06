# FIXME: Should this be the generic listener for all incoming UDP data, not just broadcasts?
# FIXME: Pro: single entry point, easier code. Con: tighter coupling of membership + consensus
defmodule Pontoon.Broadcast do
  require Logger
  use GenServer

  @broadcast_port 8213

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, :ok, opts)
  end

  def init(:ok) do
    {:ok, socket} = :gen_udp.open(@broadcast_port, [
          :binary,
          :inet,
          {:broadcast, true},
          {:active, true}
        ])

    Logger.info "called it: #{inspect socket}"

    {:ok, %{socket: socket}}
  end

  def handle_info({:udp, _socket, _ip, _port, data}, state) do
    Logger.info("yo: #{inspect data}")

    {:noreply, state}
  end

  def handle_info(other, state) do
    Logger.info("THE UNEXPECTED: #{inspect other}")
  end
end
