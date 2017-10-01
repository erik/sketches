defmodule Pronk.IRC do
  def start_connection(client) do
    command(client, "001", "sup yo")
  end

  def command(client, command, rest) when is_binary(rest) do
    send_raw(client, "#{command} #{rest}")
  end

  def command(client, command, params) when is_list(params) do
    params = Enum.join(params, " ")
    send_raw(client, "#{command} #{params}")
  end

  def send_raw(client, text) do
    :gen_tcp.send(client.socket, "#{text}\r\n")
  end

  def recv_line(client) do
    :gen_tcp.recv(client.socket, 0)
  end
end
