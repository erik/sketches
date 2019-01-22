defmodule StandupWeb.PageController do
  use StandupWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
