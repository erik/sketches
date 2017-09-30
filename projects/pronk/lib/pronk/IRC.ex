defmodule Pronk.IRC do
  def command(client, command, params) do
    params = Enum.join(params, " ")
    send_raw(client, "#{command} #{params}")
  end

  def send_raw(client, text) do
    :gen_tcp.send(client, "#{text}\r\n")
  end
end
