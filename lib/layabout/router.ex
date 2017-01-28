defmodule Layabout.Router do
  use Plug.Router

  plug :match
  plug :dispatch

  get "/" do
    send_resp(conn, 200, 'tbd html')
  end

  get "/user" do
    user = Layabout.Store.get_user_list()

    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, Poison.encode!(users))
  end

  get "/user/:user" do
    hist = Layabout.Store.build_histogram(user)

    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, Poison.encode!(hist))
  end

  match _ do
    send_resp(conn, 404, "oops")
  end
end
