defmodule Pronk.ClientConnection do
  require Logger

  defstruct [:socket, user: "unknown-nick", authed: false, caps: MapSet.new()]

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
    case Pronk.IRC.recv_line(client) do
      {:ok, line} ->
        words = line
        |> String.trim
        |> String.split(" ")

        case words do
          ["PASS", login] ->
            Logger.debug("User trying to authenticate with #{login}")

            case Pronk.UserRegistry.try_login(login) do
              nil ->
                Pronk.IRC.command(client, 464, "unknown-nick :Password incorrect")

              user ->
                client = %{client | user: user}
                Logger.debug("here: #{inspect client} #{user}")

                Pronk.IRC.start_connection(client)
                serve(client)
            end

          _ ->
            Pronk.IRC.command(client, 451, "unknown-nick :Not registered")
            serve_initial(client)
        end

      {:error, :closed} ->
        Logger.info("client disconnect: #{inspect client}")
    end
  end

  defp serve(client) do
    ## Handle all the stuff after users auth

    case Pronk.IRC.recv_line(client) do
      {:ok, line} ->
        line = String.trim(line)
        Logger.debug(">> line: #{line}")

        serve(client)

      {:error, :closed} ->
        Logger.debug("client connection closed")
    end
  end
end
