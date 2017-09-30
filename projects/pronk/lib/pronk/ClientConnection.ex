# TODO: Rename -> ClientListener?

defmodule Pronk.ClientConnection do
  require Logger

  def listen(port) do
    {:ok, socket} = :gen_tcp.listen(port,
      [:binary, packet: :line, active: false, reuseaddr: true])

    Logger.info "listening on #{port}"

    accept_loop(socket)
  end

  defp accept_loop(socket) do
    {:ok, client} = :gen_tcp.accept(socket)

    {:ok, pid} = Task.Supervisor.start_child(Pronk.TaskSupervisor, fn ->
      serve_initial(client)
    end)

    :ok = :gen_tcp.controlling_process(client, pid)

    accept_loop(socket)
  end

  ## Handle all the stuff before users auth
  defp serve_initial(client) do
    case :gen_tcp.recv(client, 0) do
      {:ok, line} ->
        words = line
        |> String.trim
        |> String.split(" ")

        case words do
          ["PASS", login] ->
            Logger.info "User trying to authenticate with #{login}"
            case Pronk.UserRegistry.user_from_login(login) do
              nil ->
                Pronk.IRC.command(client, 464, [":Password incorrect"])

              user ->
                serve(client, user)
            end

          _ ->
            Pronk.IRC.command(client, 451, [":Not registered"])
            serve_initial(client)
        end

      {:error, :closed} ->
        Logger.info("client disconnect: #{inspect client}")
    end
  end

  defp serve(client, user) do
    ## Handle all the stuff after users auth
    Logger.info("here: #{inspect client} #{user}")
    Pronk.IRC.command(client, "001", [":Let's get pronking!"])
  end
end
