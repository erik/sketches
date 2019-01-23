defmodule StandupWeb.UpdateController do
  use StandupWeb, :controller

  alias Standup.Post
  alias Standup.Post.Update

  def index(conn, _params) do
    updates = Post.list_updates()
    render(conn, "index.html", updates: updates)
  end

  def new(conn, _params) do
    changeset = Post.change_update(%Update{})
    render(conn, "new.html", changeset: changeset)
  end

  def create(conn, %{"update" => update_params}) do
    case Post.create_update(update_params) do
      {:ok, update} ->
        conn
        |> put_flash(:info, "Update created successfully.")
        |> redirect(to: Routes.update_path(conn, :show, update))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "new.html", changeset: changeset)
    end
  end

  def show(conn, %{"id" => id}) do
    update = Post.get_update!(id)
    render(conn, "show.html", update: update)
  end

  def edit(conn, %{"id" => id}) do
    update = Post.get_update!(id)
    changeset = Post.change_update(update)
    render(conn, "edit.html", update: update, changeset: changeset)
  end

  def update(conn, %{"id" => id, "update" => update_params}) do
    update = Post.get_update!(id)

    case Post.update_update(update, update_params) do
      {:ok, update} ->
        conn
        |> put_flash(:info, "Update updated successfully.")
        |> redirect(to: Routes.update_path(conn, :show, update))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "edit.html", update: update, changeset: changeset)
    end
  end

  def delete(conn, %{"id" => id}) do
    update = Post.get_update!(id)
    {:ok, _update} = Post.delete_update(update)

    conn
    |> put_flash(:info, "Update deleted successfully.")
    |> redirect(to: Routes.update_path(conn, :index))
  end
end
