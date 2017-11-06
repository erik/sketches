# FIXME: Should this be the generic listener for all incoming UDP data, not just broadcasts?
# FIXME: Pro: single entry point, easier code. Con: tighter coupling of membership + consensus
defmodule Pontoon.Broadcast do
  require Logger
  use GenServer

  @broadcast_port 8213

  defmodule Message do
    @derive [Poison.Encoder]
    defstruct [:type, :data]
  end

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

  def handle_info({:udp, _socket, ip, port, data}, state) do
    msg = Poison.decode!(data, as: %Message{})

    Logger.info("Inbound: #{inspect msg} from #{inspect ip}:#{inspect port}")

    case msg.type do
      "PING" ->
        Pontoon.Membership.add_member(port)
      "QUIT" ->
        Poontoon.Membership.remove_member(port)
    end

    {:noreply, state}
  end

  def handle_info(other, state) do
    Logger.info("THE UNEXPECTED: #{inspect other}")
  end
end
