defmodule VitalWeb.SignInController do
  use VitalWeb, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end

  def signin(conn, %{}) do
    conn
  end

  def register(conn, %{}) do
    conn
  end
end
