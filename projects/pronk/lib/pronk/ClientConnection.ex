defmodule Pronk.ClientConnection do
  require Logger

  defstruct [:socket, user: "unknown-nick", authed: false, caps: MapSet.new()]

  defmodule IRC do
    def start_connection(client) do
      command(client, "001", [client.user, "sup yo"])
    end

    def command(client, cmd, params) when is_list(params) do
      first_params = Enum.drop(params, -1) |> Enum.join(" ")
      last_param = List.last(params)

      send_raw(client, ":pronk.irc #{cmd} #{first_params} :#{last_param}")
    end

    def send_raw(client, text) do
      :gen_tcp.send(client.socket, "#{text}\r\n")
    end

    def recv_line(client) do
      :gen_tcp.recv(client.socket, 0)
    end
  end

  def listen(port) do
    {:ok, socket} = :gen_tcp.listen(port,
      [:binary, packet: :line, active: false, reuseaddr: true])

    Logger.info("listening on #{port}")

    accept_loop(socket)
  end

  defp accept_loop(socket) do
    {:ok, client_socket} = :gen_tcp.accept(socket)

    {:ok, pid} = Task.Supervisor.start_child(Pronk.TaskSupervisor, fn ->
      serve_initial(%Pronk.ClientConnection{socket: client_socket})
    end)

    :ok = :gen_tcp.controlling_process(client_socket, pid)

    accept_loop(socket)
  end

  ## Handle all the stuff before users auth
  defp serve_initial(client) do
    case IRC.recv_line(client) do
      {:ok, line} ->
        words = line
        |> String.trim
        |> String.split(" ")

        case words do
          ["PASS", login] ->
            Logger.debug("User trying to authenticate with #{login}")

            case Pronk.UserRegistry.try_login(login) do
              nil ->
                IRC.command(client, 464, ["Password incorrect"])

              user ->
                client = %{client | user: user}
                Logger.debug("here: #{inspect client} #{user}")

                IRC.start_connection(client)
                serve(client)
            end

          _ ->
            IRC.command(client, 451, ["Not registered"])
            serve_initial(client)
        end

      {:error, :closed} ->
        Logger.info("client disconnect: #{inspect client}")
    end
  end

  defp serve(client) do
    ## Handle all the stuff after users auth

    case IRC.recv_line(client) do
      {:ok, line} ->
        case String.trim(line) |> IRC.parse do
          {:error, command} ->
            IRC.command(client, 421, ["Unknown command"])

          {:ok, nil} ->
            nil
        end


        if line != "" do
          Logger.debug(">> line: #{line}")
          IRC.handle_line(line)
        end

        serve(client)

      {:error, :closed} ->
        Logger.debug("client connection closed")
    end
  end
end
