defmodule StandupWeb.AuthController do
  use StandupWeb, :controller

  alias Standup.Accounts
  alias Standup.Accounts.User
  alias Standup.Authors

  alias StandupWeb.Guardian

  def new(conn, _params) do
    conn
    |> assign(:changeset, Accounts.change_user(%User{}))
    |> render("new.html")
  end

  def create(conn, %{"user" => %{"email" => email}}) do
    case Accounts.get_or_create_user_by_email(email) do
      {:ok, user} ->
        {:ok, token, _claims} = Guardian.encode_magic(user)
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
    |> redirect(to: Routes.page_path(conn, :index))
  end

  def callback(conn, %{"magic_token" => magic_token}) do
    case Guardian.decode_magic(magic_token) do
      {:ok, user, _claims} ->
        # If this is the first login (no author), we want to redirect to the
        # author page first.
        route =
          case Authors.get_author_by_user_id(user.id) do
            nil ->
              Routes.author_path(conn, :new)

            _ ->
              Routes.page_path(conn, :index)
          end

        conn
        |> Guardian.Plug.sign_in(user)
        |> redirect(to: route)

      _ ->
        conn
        |> put_flash(:error, "Invalid magic link.")
        |> redirect(to: Routes.auth_path(conn, :new))
    end
  end

  def auth_error(conn, _error, _opts) do
    conn
    |> put_flash(:error, "Failed to sign in.")
    |> redirect(to: Routes.auth_path(conn, :new))
    |> halt()
  end
end
