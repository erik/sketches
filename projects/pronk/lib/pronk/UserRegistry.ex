defmodule Pronk.UserRegistry do
  def start_link do
    Agent.start_link(fn -> nil end, name: __MODULE__)
  end

  def user_from_login(login) do
    case String.split(login, ":", parts: 2) do
      ["asdf", "asdf"] ->
        "asdf"

      [_user, _password] ->
        # TODO: Write me
        nil

      _ ->
        nil
    end
  end

  def get_user(_name) do

  end
end
