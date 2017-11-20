defmodule Pontoon.Multicast do
  require Logger
  use GenServer

  @announce_interval_ms 3 * 1000
  @multicast_address {224, 0, 0, 251}

  defmodule Message do
    @derive [Poison.Encoder]
    defstruct [:name, :port, :type, :data]
  end

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts)
  end

  def init(opts) do

    {:ok, socket} = :gen_udp.open(opts[:multicast_port], [
          :binary,
          ip: @multicast_address,
          multicast_ttl: 4,
          multicast_loop: true,
          broadcast: true,
          add_membership: {@multicast_address, {0,0,0,0}},
          reuseaddr: true,
          active: true,
        ])

    Process.send_after(self(), :announce_self, @announce_interval_ms)

    {:ok, %{socket: socket,
            rpc_port: opts[:rpc_port],
            multicast_port: opts[:multicast_port]}}
  end

  # Make sure we send a quit message before we die
  def terminate(_reason, state) do
    %Message{type: "QUIT", name: Pontoon.Membership.get_own_name()}
    |> Poison.encode!
    |> send_multicast(state)
  end

  def send_multicast(message, state) do
    {:ok, sock} = :gen_udp.open(0, [:binary])
    :ok = :gen_udp.send(sock, @multicast_address, state[:multicast_port], message)
    :ok = :gen_udp.close(sock)
  end

  def handle_info(:announce_self, state) do
    %Message{
      type: "PING",
      data: "",
      port: state[:rpc_port],
      name: Pontoon.Membership.get_own_name()
    }
    |> Poison.encode!
    |> send_multicast(state)

    Process.send_after(self(), :announce_self, @announce_interval_ms)

    {:noreply, state}
  end

  def handle_info({:udp, _socket, ip, _port, data}, state) do
    msg = Poison.decode!(data, as: %Message{})

    case msg.type do
      "PING" ->
        member = %Pontoon.Member{address: ip,
                                 port: msg.port,
                                 name: msg.name,
                                 last_seen: DateTime.utc_now}

        if msg.name != Pontoon.Membership.get_own_name()  do
          Pontoon.Membership.add_member(msg.name, member)
        end

      "QUIT" ->
        Pontoon.Membership.remove_member(msg.name)
    end

    {:noreply, state}
  end

  def handle_info(other, _state) do
    Logger.info("THE UNEXPECTED: #{inspect other}")
  end
end
