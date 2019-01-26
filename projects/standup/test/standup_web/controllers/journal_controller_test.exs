defmodule StandupWeb.JournalControllerTest do
  use StandupWeb.ConnCase

  alias Standup.Content

  @create_attrs %{completed_at: "2010-04-17T14:00:00Z", description: "some description", public: true, started_at: "2010-04-17T14:00:00Z", tags: [], title: "some title"}
  @update_attrs %{completed_at: "2011-05-18T15:01:01Z", description: "some updated description", public: false, started_at: "2011-05-18T15:01:01Z", tags: [], title: "some updated title"}
  @invalid_attrs %{completed_at: nil, description: nil, public: nil, started_at: nil, tags: nil, title: nil}

  def fixture(:journal) do
    {:ok, journal} = Content.create_journal(@create_attrs)
    journal
  end

  describe "index" do
    test "lists all journals", %{conn: conn} do
      conn = get(conn, Routes.journal_path(conn, :index))
      assert html_response(conn, 200) =~ "Listing Journals"
    end
  end

  describe "new journal" do
    test "renders form", %{conn: conn} do
      conn = get(conn, Routes.journal_path(conn, :new))
      assert html_response(conn, 200) =~ "New Journal"
    end
  end

  describe "create journal" do
    test "redirects to show when data is valid", %{conn: conn} do
      conn = post(conn, Routes.journal_path(conn, :create), journal: @create_attrs)

      assert %{id: id} = redirected_params(conn)
      assert redirected_to(conn) == Routes.journal_path(conn, :show, id)

      conn = get(conn, Routes.journal_path(conn, :show, id))
      assert html_response(conn, 200) =~ "Show Journal"
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, Routes.journal_path(conn, :create), journal: @invalid_attrs)
      assert html_response(conn, 200) =~ "New Journal"
    end
  end

  describe "edit journal" do
    setup [:create_journal]

    test "renders form for editing chosen journal", %{conn: conn, journal: journal} do
      conn = get(conn, Routes.journal_path(conn, :edit, journal))
      assert html_response(conn, 200) =~ "Edit Journal"
    end
  end

  describe "update journal" do
    setup [:create_journal]

    test "redirects when data is valid", %{conn: conn, journal: journal} do
      conn = put(conn, Routes.journal_path(conn, :update, journal), journal: @update_attrs)
      assert redirected_to(conn) == Routes.journal_path(conn, :show, journal)

      conn = get(conn, Routes.journal_path(conn, :show, journal))
      assert html_response(conn, 200) =~ "some updated description"
    end

    test "renders errors when data is invalid", %{conn: conn, journal: journal} do
      conn = put(conn, Routes.journal_path(conn, :update, journal), journal: @invalid_attrs)
      assert html_response(conn, 200) =~ "Edit Journal"
    end
  end

  describe "delete journal" do
    setup [:create_journal]

    test "deletes chosen journal", %{conn: conn, journal: journal} do
      conn = delete(conn, Routes.journal_path(conn, :delete, journal))
      assert redirected_to(conn) == Routes.journal_path(conn, :index)
      assert_error_sent 404, fn ->
        get(conn, Routes.journal_path(conn, :show, journal))
      end
    end
  end

  defp create_journal(_) do
    journal = fixture(:journal)
    {:ok, journal: journal}
  end
end
