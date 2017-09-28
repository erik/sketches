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
      {:ok, line} -> _
        words = line
        |> String.trim " "
        |> String.split " "

        case words do
          ["PASS", user] ->
            Logger.info "User trying to authenticate with #{user}"

          :else ->
            :gen_tcp.send(socket, "You need to authenticate before doing that.\r\n")
        end

      {:error, :closed} ->
        Logger.info("client disconnect: #{inspect client}")
    end
  end

  defp serve(client) do
    ## Handle all the stuff after users auth
  end
end
