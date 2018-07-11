defmodule VitalsignsWeb.PageController do
  use VitalsignsWeb, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
