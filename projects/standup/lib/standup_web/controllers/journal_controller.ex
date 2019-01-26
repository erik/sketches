defmodule StandupWeb.JournalController do
  use StandupWeb, :controller

  alias Standup.Content
  alias Standup.Content.Journal

  def index(conn, _params) do
    journals = Content.list_journals()
    render(conn, "index.html", journals: journals)
  end

  def new(conn, _params) do
    changeset = Content.change_journal(%Journal{})
    render(conn, "new.html", changeset: changeset)
  end

  def create(conn, %{"journal" => journal_params}) do
    case Content.create_journal(journal_params) do
      {:ok, journal} ->
        conn
        |> put_flash(:info, "Journal created successfully.")
        |> redirect(to: Routes.journal_path(conn, :show, journal))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "new.html", changeset: changeset)
    end
  end

  def show(conn, %{"id" => id}) do
    journal = Content.get_journal!(id)
    render(conn, "show.html", journal: journal)
  end

  def edit(conn, %{"id" => id}) do
    journal = Content.get_journal!(id)
    changeset = Content.change_journal(journal)
    render(conn, "edit.html", journal: journal, changeset: changeset)
  end

  def update(conn, %{"id" => id, "journal" => journal_params}) do
    journal = Content.get_journal!(id)

    case Content.update_journal(journal, journal_params) do
      {:ok, journal} ->
        conn
        |> put_flash(:info, "Journal updated successfully.")
        |> redirect(to: Routes.journal_path(conn, :show, journal))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "edit.html", journal: journal, changeset: changeset)
    end
  end

  def delete(conn, %{"id" => id}) do
    journal = Content.get_journal!(id)
    {:ok, _journal} = Content.delete_journal(journal)

    conn
    |> put_flash(:info, "Journal deleted successfully.")
    |> redirect(to: Routes.journal_path(conn, :index))
  end
end
