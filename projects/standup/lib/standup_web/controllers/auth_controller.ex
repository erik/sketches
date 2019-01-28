defmodule StandupWeb.AuthController do
  use StandupWeb, :controller

  alias Standup.Accounts
  alias Standup.Accounts.User

  def new(conn, _params) do
    conn
    |> assign(:changeset, Accounts.change_user(%User{}))
    |> render("new.html")
  end

  def create(conn, %{"user" => %{"email" => email}}) do
    case Accounts.get_or_create_by_email(email) do
      {:ok, user} ->
        render(conn, "create.html")

      {:error, changeset} ->
        conn
        |> assign(:changeset, changeset)
        |> render("new.html")
    end
  end

  def destroy(conn, _params) do
    conn
  end

  def callback(conn, _params) do
    conn
  end
end
