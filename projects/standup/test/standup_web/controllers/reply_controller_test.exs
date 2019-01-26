defmodule StandupWeb.ReplyControllerTest do
  use StandupWeb.ConnCase

  alias Standup.Social

  @create_attrs %{body: "some body"}
  @update_attrs %{body: "some updated body"}
  @invalid_attrs %{body: nil}

  def fixture(:reply) do
    {:ok, reply} = Social.create_reply(@create_attrs)
    reply
  end

  describe "index" do
    test "lists all replies", %{conn: conn} do
      conn = get(conn, Routes.reply_path(conn, :index))
      assert html_response(conn, 200) =~ "Listing Replies"
    end
  end

  describe "new reply" do
    test "renders form", %{conn: conn} do
      conn = get(conn, Routes.reply_path(conn, :new))
      assert html_response(conn, 200) =~ "New Reply"
    end
  end

  describe "create reply" do
    test "redirects to show when data is valid", %{conn: conn} do
      conn = post(conn, Routes.reply_path(conn, :create), reply: @create_attrs)

      assert %{id: id} = redirected_params(conn)
      assert redirected_to(conn) == Routes.reply_path(conn, :show, id)

      conn = get(conn, Routes.reply_path(conn, :show, id))
      assert html_response(conn, 200) =~ "Show Reply"
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, Routes.reply_path(conn, :create), reply: @invalid_attrs)
      assert html_response(conn, 200) =~ "New Reply"
    end
  end

  describe "edit reply" do
    setup [:create_reply]

    test "renders form for editing chosen reply", %{conn: conn, reply: reply} do
      conn = get(conn, Routes.reply_path(conn, :edit, reply))
      assert html_response(conn, 200) =~ "Edit Reply"
    end
  end

  describe "update reply" do
    setup [:create_reply]

    test "redirects when data is valid", %{conn: conn, reply: reply} do
      conn = put(conn, Routes.reply_path(conn, :update, reply), reply: @update_attrs)
      assert redirected_to(conn) == Routes.reply_path(conn, :show, reply)

      conn = get(conn, Routes.reply_path(conn, :show, reply))
      assert html_response(conn, 200) =~ "some updated body"
    end

    test "renders errors when data is invalid", %{conn: conn, reply: reply} do
      conn = put(conn, Routes.reply_path(conn, :update, reply), reply: @invalid_attrs)
      assert html_response(conn, 200) =~ "Edit Reply"
    end
  end

  describe "delete reply" do
    setup [:create_reply]

    test "deletes chosen reply", %{conn: conn, reply: reply} do
      conn = delete(conn, Routes.reply_path(conn, :delete, reply))
      assert redirected_to(conn) == Routes.reply_path(conn, :index)
      assert_error_sent 404, fn ->
        get(conn, Routes.reply_path(conn, :show, reply))
      end
    end
  end

  defp create_reply(_) do
    reply = fixture(:reply)
    {:ok, reply: reply}
  end
end
