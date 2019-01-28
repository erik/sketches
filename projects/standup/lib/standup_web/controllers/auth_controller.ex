defmodule StandupWeb.AuthController do
  use StandupWeb, :controller

  alias Standup.Accounts
  alias Standup.Accounts.User

  alias StandupWeb.Guardian

  def new(conn, _params) do
    conn
    |> assign(:changeset, Accounts.change_user(%User{}))
    |> render("new.html")
  end

  def create(conn, %{"user" => %{"email" => email}}) do
    case Accounts.get_or_create_user_by_email(email) do
      {:ok, user} ->
        {:ok, token, claims} = Guardian.encode_magic(user)
        render(conn, "create.html", magic: token)

      {:error, changeset} ->
        conn
        |> assign(:changeset, changeset)
        |> render("new.html")
    end
  end

  def destroy(conn, _params) do
    conn
    |> Guardian.Plug.sign_out()
    |> redirect(to: Helpers.page_path(conn, :index))
  end

  def callback(conn, _params) do
    conn
  end

  def auth_error(conn, error, _opts) do
    conn
    |> put_flash(:error, "Failed to sign in.")
    |> redirect(to: Helpers.auth_path(conn, :new))
    |> halt()
  end
end
