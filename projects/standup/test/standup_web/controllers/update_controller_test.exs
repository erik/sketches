defmodule StandupWeb.UpdateControllerTest do
  use StandupWeb.ConnCase

  alias Standup.Post

  @create_attrs %{body: "some body", title: "some title"}
  @update_attrs %{body: "some updated body", title: "some updated title"}
  @invalid_attrs %{body: nil, title: nil}

  def fixture(:update) do
    {:ok, update} = Post.create_update(@create_attrs)
    update
  end

  describe "index" do
    test "lists all updates", %{conn: conn} do
      conn = get(conn, Routes.update_path(conn, :index))
      assert html_response(conn, 200) =~ "Listing Updates"
    end
  end

  describe "new update" do
    test "renders form", %{conn: conn} do
      conn = get(conn, Routes.update_path(conn, :new))
      assert html_response(conn, 200) =~ "New Update"
    end
  end

  describe "create update" do
    test "redirects to show when data is valid", %{conn: conn} do
      conn = post(conn, Routes.update_path(conn, :create), update: @create_attrs)

      assert %{id: id} = redirected_params(conn)
      assert redirected_to(conn) == Routes.update_path(conn, :show, id)

      conn = get(conn, Routes.update_path(conn, :show, id))
      assert html_response(conn, 200) =~ "Show Update"
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, Routes.update_path(conn, :create), update: @invalid_attrs)
      assert html_response(conn, 200) =~ "New Update"
    end
  end

  describe "edit update" do
    setup [:create_update]

    test "renders form for editing chosen update", %{conn: conn, update: update} do
      conn = get(conn, Routes.update_path(conn, :edit, update))
      assert html_response(conn, 200) =~ "Edit Update"
    end
  end

  describe "update update" do
    setup [:create_update]

    test "redirects when data is valid", %{conn: conn, update: update} do
      conn = put(conn, Routes.update_path(conn, :update, update), update: @update_attrs)
      assert redirected_to(conn) == Routes.update_path(conn, :show, update)

      conn = get(conn, Routes.update_path(conn, :show, update))
      assert html_response(conn, 200) =~ "some updated body"
    end

    test "renders errors when data is invalid", %{conn: conn, update: update} do
      conn = put(conn, Routes.update_path(conn, :update, update), update: @invalid_attrs)
      assert html_response(conn, 200) =~ "Edit Update"
    end
  end

  describe "delete update" do
    setup [:create_update]

    test "deletes chosen update", %{conn: conn, update: update} do
      conn = delete(conn, Routes.update_path(conn, :delete, update))
      assert redirected_to(conn) == Routes.update_path(conn, :index)
      assert_error_sent 404, fn ->
        get(conn, Routes.update_path(conn, :show, update))
      end
    end
  end

  defp create_update(_) do
    update = fixture(:update)
    {:ok, update: update}
  end
end
