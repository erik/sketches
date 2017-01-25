defmodule Layabout.Router do
  use Plug.Router

  plug :match
  plug :dispatch

  get "/" do
    send_resp(conn, 200, 'tbd html')
  end

  get "/heat/:user" do
    hist = Layabout.Store.get_histogram(user)

    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, Poison.encode!(hist))
  end

  match _ do
    send_resp(conn, 404, "oops")
  end
end
