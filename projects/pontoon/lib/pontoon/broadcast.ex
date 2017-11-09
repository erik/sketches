# FIXME: Should this be the generic listener for all incoming UDP data, not just broadcasts?
# FIXME: Pro: single entry point, easier code. Con: tighter coupling of membership + consensus
defmodule Pontoon.Broadcast do
  require Logger
  use GenServer

  @announce_interval_ms 3 * 1000
  @broadcast_address {255, 255, 255, 255}
  @broadcast_port (System.get_env("BROADCAST_PORT") || "8213") |> String.to_integer

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

    Process.send_after(self(), :announce_self, @announce_interval_ms)

    {:ok, %{socket: socket}}
  end

  def broadcast(message) do
    Logger.warn("Unimplemented: broadcast message")
  end

  def handle_info(:announce_self, state) do
    message = %Message{type: "PING", data: ""}
    |> Poison.encode!

    :ok = :gen_udp.send(state.socket, @broadcast_address, @broadcast_port, message)

    Process.send_after(self(), :announce_self, @announce_interval_ms)

    {:noreply, state}
  end

  def handle_info({:udp, _socket, ip, port, data}, state) do
    msg = Poison.decode!(data, as: %Message{})

    # Key format is 127.0.0.1:9999
    key = "#{:inet_parse.ntoa(ip)}:#{port}"

    case msg.type do
      "PING" ->
        member = %Pontoon.Member{address: ip,
                                 port: port,
                                 last_seen: DateTime.utc_now}
        Pontoon.Membership.add_member(key, member)
      "QUIT" ->
        Pontoon.Membership.remove_member(key)
    end

    {:noreply, state}
  end

  def handle_info(other, _state) do
    Logger.info("THE UNEXPECTED: #{inspect other}")
  end
end
