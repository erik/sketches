defmodule VitalsignsWeb.AttachmentController do
  use VitalsignsWeb, :controller

  alias Vitalsigns.Web
  alias Vitalsigns.Web.Attachment

  def index(conn, _params) do
    attachments = Web.list_attachments()
    render(conn, "index.html", attachments: attachments)
  end

  def new(conn, _params) do
    changeset = Web.change_attachment(%Attachment{})
    render(conn, "new.html", changeset: changeset)
  end

  def create(conn, %{"attachment" => attachment_params}) do
    case Web.create_attachment(attachment_params) do
      {:ok, attachment} ->
        conn
        |> put_flash(:info, "Attachment created successfully.")
        |> redirect(to: attachment_path(conn, :show, attachment))
      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "new.html", changeset: changeset)
    end
  end

  def show(conn, %{"id" => id}) do
    attachment = Web.get_attachment!(id)
    render(conn, "show.html", attachment: attachment)
  end

  def edit(conn, %{"id" => id}) do
    attachment = Web.get_attachment!(id)
    changeset = Web.change_attachment(attachment)
    render(conn, "edit.html", attachment: attachment, changeset: changeset)
  end

  def update(conn, %{"id" => id, "attachment" => attachment_params}) do
    attachment = Web.get_attachment!(id)

    case Web.update_attachment(attachment, attachment_params) do
      {:ok, attachment} ->
        conn
        |> put_flash(:info, "Attachment updated successfully.")
        |> redirect(to: attachment_path(conn, :show, attachment))
      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "edit.html", attachment: attachment, changeset: changeset)
    end
  end

  def delete(conn, %{"id" => id}) do
    attachment = Web.get_attachment!(id)
    {:ok, _attachment} = Web.delete_attachment(attachment)

    conn
    |> put_flash(:info, "Attachment deleted successfully.")
    |> redirect(to: attachment_path(conn, :index))
  end
end
