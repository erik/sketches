defmodule StandupWeb.ReplyController do
  use StandupWeb, :controller

  alias Standup.Social
  alias Standup.Social.Reply

  def index(conn, _params) do
    replies = Social.list_replies()
    render(conn, "index.html", replies: replies)
  end

  def new(conn, _params) do
    changeset = Social.change_reply(%Reply{})
    render(conn, "new.html", changeset: changeset)
  end

  def create(conn, %{"reply" => reply_params}) do
    case Social.create_reply(reply_params) do
      {:ok, reply} ->
        conn
        |> put_flash(:info, "Reply created successfully.")
        |> redirect(to: Routes.reply_path(conn, :show, reply))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "new.html", changeset: changeset)
    end
  end

  def show(conn, %{"id" => id}) do
    reply = Social.get_reply!(id)
    render(conn, "show.html", reply: reply)
  end

  def edit(conn, %{"id" => id}) do
    reply = Social.get_reply!(id)
    changeset = Social.change_reply(reply)
    render(conn, "edit.html", reply: reply, changeset: changeset)
  end

  def update(conn, %{"id" => id, "reply" => reply_params}) do
    reply = Social.get_reply!(id)

    case Social.update_reply(reply, reply_params) do
      {:ok, reply} ->
        conn
        |> put_flash(:info, "Reply updated successfully.")
        |> redirect(to: Routes.reply_path(conn, :show, reply))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "edit.html", reply: reply, changeset: changeset)
    end
  end

  def delete(conn, %{"id" => id}) do
    reply = Social.get_reply!(id)
    {:ok, _reply} = Social.delete_reply(reply)

    conn
    |> put_flash(:info, "Reply deleted successfully.")
    |> redirect(to: Routes.reply_path(conn, :index))
  end
end
