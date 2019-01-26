defmodule StandupWeb.EntryController do
  use StandupWeb, :controller

  alias Standup.Content
  alias Standup.Content.Entry

  def index(conn, _params) do
    entries = Content.list_entries()
    render(conn, "index.html", entries: entries)
  end

  def new(conn, _params) do
    changeset = Content.change_entry(%Entry{})
    render(conn, "new.html", changeset: changeset)
  end

  def create(conn, %{"entry" => entry_params}) do
    case Content.create_entry(entry_params) do
      {:ok, entry} ->
        conn
        |> put_flash(:info, "Entry created successfully.")
        |> redirect(to: Routes.entry_path(conn, :show, entry))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "new.html", changeset: changeset)
    end
  end

  def show(conn, %{"id" => id}) do
    entry = Content.get_entry!(id)
    render(conn, "show.html", entry: entry)
  end

  def edit(conn, %{"id" => id}) do
    entry = Content.get_entry!(id)
    changeset = Content.change_entry(entry)
    render(conn, "edit.html", entry: entry, changeset: changeset)
  end

  def update(conn, %{"id" => id, "entry" => entry_params}) do
    entry = Content.get_entry!(id)

    case Content.update_entry(entry, entry_params) do
      {:ok, entry} ->
        conn
        |> put_flash(:info, "Entry updated successfully.")
        |> redirect(to: Routes.entry_path(conn, :show, entry))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "edit.html", entry: entry, changeset: changeset)
    end
  end

  def delete(conn, %{"id" => id}) do
    entry = Content.get_entry!(id)
    {:ok, _entry} = Content.delete_entry(entry)

    conn
    |> put_flash(:info, "Entry deleted successfully.")
    |> redirect(to: Routes.entry_path(conn, :index))
  end
end
