defmodule Pronk.ClientConnection do
  require Logger

  defstruct [:socket, user: "unknown-nick", authed: false, caps: MapSet.new()]

  defp start_connection(client) do
    command(client, "001", [client.user, "sup yo"])
  end

  defp command(client, cmd, params) when is_list(params) do
    first_params = Enum.drop(params, -1) |> Enum.join(" ")
    last_param = List.last(params)

    send_raw(client, ":pronk.irc #{cmd} #{first_params} :#{last_param}")
  end

  defp send_raw(client, text) do
    :gen_tcp.send(client.socket, "#{text}\r\n")
  end

  defp recv_line(client) do
    :gen_tcp.recv(client.socket, 0)
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
    case recv_line(client) do
      {:ok, line} ->
        words = line
        |> String.trim
        |> String.split(" ")

        case words do
          ["PASS", login] ->
            Logger.debug("User trying to authenticate with #{login}")

            case Pronk.UserRegistry.try_login(login) do
              nil ->
                command(client, 464, ["Password incorrect"])

              user ->
                client = %{client | user: user}
                Logger.debug("here: #{inspect client} #{user}")

                start_connection(client)
                serve(client)
            end

          _ ->
            command(client, 451, ["Not registered"])
            serve_initial(client)
        end

      {:error, :closed} ->
        Logger.info("client disconnect: #{inspect client}")
    end
  end

  defp serve(client) do
    ## Handle all the stuff after users auth

    case recv_line(client) do
      {:ok, line} ->
        case String.trim(line) |> Pronk.IRC.parse do
          # If the line is empty, do nothing
          nil ->
            nil

          line ->
            Logger.debug(">> #{inspect line}")
            handle_line(client, line)
        end

        serve(client)

      {:error, :closed} ->
        Logger.debug("client connection closed")
    end
  end

  defp handle_line(client, %Pronk.IRC.Line{command: "PING"}) do
    send_raw(client, "PONG :pronk.irc")
  end

  defp handle_line(client, line) do
    command(client, 421, ["Unknown command: #{line.command}"])
  end
end
