defmodule VitalsignsWeb.AttachmentControllerTest do
  use VitalsignsWeb.ConnCase

  alias Vitalsigns.Web

  @create_attrs %{body: "some body", caption: "some caption", kind: "some kind", url: "some url"}
  @update_attrs %{body: "some updated body", caption: "some updated caption", kind: "some updated kind", url: "some updated url"}
  @invalid_attrs %{body: nil, caption: nil, kind: nil, url: nil}

  def fixture(:attachment) do
    {:ok, attachment} = Web.create_attachment(@create_attrs)
    attachment
  end

  describe "index" do
    test "lists all attachments", %{conn: conn} do
      conn = get conn, attachment_path(conn, :index)
      assert html_response(conn, 200) =~ "Listing Attachments"
    end
  end

  describe "new attachment" do
    test "renders form", %{conn: conn} do
      conn = get conn, attachment_path(conn, :new)
      assert html_response(conn, 200) =~ "New Attachment"
    end
  end

  describe "create attachment" do
    test "redirects to show when data is valid", %{conn: conn} do
      conn = post conn, attachment_path(conn, :create), attachment: @create_attrs

      assert %{id: id} = redirected_params(conn)
      assert redirected_to(conn) == attachment_path(conn, :show, id)

      conn = get conn, attachment_path(conn, :show, id)
      assert html_response(conn, 200) =~ "Show Attachment"
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post conn, attachment_path(conn, :create), attachment: @invalid_attrs
      assert html_response(conn, 200) =~ "New Attachment"
    end
  end

  describe "edit attachment" do
    setup [:create_attachment]

    test "renders form for editing chosen attachment", %{conn: conn, attachment: attachment} do
      conn = get conn, attachment_path(conn, :edit, attachment)
      assert html_response(conn, 200) =~ "Edit Attachment"
    end
  end

  describe "update attachment" do
    setup [:create_attachment]

    test "redirects when data is valid", %{conn: conn, attachment: attachment} do
      conn = put conn, attachment_path(conn, :update, attachment), attachment: @update_attrs
      assert redirected_to(conn) == attachment_path(conn, :show, attachment)

      conn = get conn, attachment_path(conn, :show, attachment)
      assert html_response(conn, 200) =~ "some updated body"
    end

    test "renders errors when data is invalid", %{conn: conn, attachment: attachment} do
      conn = put conn, attachment_path(conn, :update, attachment), attachment: @invalid_attrs
      assert html_response(conn, 200) =~ "Edit Attachment"
    end
  end

  describe "delete attachment" do
    setup [:create_attachment]

    test "deletes chosen attachment", %{conn: conn, attachment: attachment} do
      conn = delete conn, attachment_path(conn, :delete, attachment)
      assert redirected_to(conn) == attachment_path(conn, :index)
      assert_error_sent 404, fn ->
        get conn, attachment_path(conn, :show, attachment)
      end
    end
  end

  defp create_attachment(_) do
    attachment = fixture(:attachment)
    {:ok, attachment: attachment}
  end
end
