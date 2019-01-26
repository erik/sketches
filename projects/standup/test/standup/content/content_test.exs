defmodule Standup.ContentTest do
  use Standup.DataCase

  alias Standup.Content

  describe "authors" do
    alias Standup.Content.Author

    @valid_attrs %{bio: "some bio", image_url: "some image_url", location: "some location", name: "some name"}
    @update_attrs %{bio: "some updated bio", image_url: "some updated image_url", location: "some updated location", name: "some updated name"}
    @invalid_attrs %{bio: nil, image_url: nil, location: nil, name: nil}

    def author_fixture(attrs \\ %{}) do
      {:ok, author} =
        attrs
        |> Enum.into(@valid_attrs)
        |> Content.create_author()

      author
    end

    test "list_authors/0 returns all authors" do
      author = author_fixture()
      assert Content.list_authors() == [author]
    end

    test "get_author!/1 returns the author with given id" do
      author = author_fixture()
      assert Content.get_author!(author.id) == author
    end

    test "create_author/1 with valid data creates a author" do
      assert {:ok, %Author{} = author} = Content.create_author(@valid_attrs)
      assert author.bio == "some bio"
      assert author.image_url == "some image_url"
      assert author.location == "some location"
      assert author.name == "some name"
    end

    test "create_author/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Content.create_author(@invalid_attrs)
    end

    test "update_author/2 with valid data updates the author" do
      author = author_fixture()
      assert {:ok, %Author{} = author} = Content.update_author(author, @update_attrs)
      assert author.bio == "some updated bio"
      assert author.image_url == "some updated image_url"
      assert author.location == "some updated location"
      assert author.name == "some updated name"
    end

    test "update_author/2 with invalid data returns error changeset" do
      author = author_fixture()
      assert {:error, %Ecto.Changeset{}} = Content.update_author(author, @invalid_attrs)
      assert author == Content.get_author!(author.id)
    end

    test "delete_author/1 deletes the author" do
      author = author_fixture()
      assert {:ok, %Author{}} = Content.delete_author(author)
      assert_raise Ecto.NoResultsError, fn -> Content.get_author!(author.id) end
    end

    test "change_author/1 returns a author changeset" do
      author = author_fixture()
      assert %Ecto.Changeset{} = Content.change_author(author)
    end
  end

  describe "journals" do
    alias Standup.Content.Journal

    @valid_attrs %{completed_at: "2010-04-17T14:00:00Z", description: "some description", public: true, started_at: "2010-04-17T14:00:00Z", tags: [], title: "some title"}
    @update_attrs %{completed_at: "2011-05-18T15:01:01Z", description: "some updated description", public: false, started_at: "2011-05-18T15:01:01Z", tags: [], title: "some updated title"}
    @invalid_attrs %{completed_at: nil, description: nil, public: nil, started_at: nil, tags: nil, title: nil}

    def journal_fixture(attrs \\ %{}) do
      {:ok, journal} =
        attrs
        |> Enum.into(@valid_attrs)
        |> Content.create_journal()

      journal
    end

    test "list_journals/0 returns all journals" do
      journal = journal_fixture()
      assert Content.list_journals() == [journal]
    end

    test "get_journal!/1 returns the journal with given id" do
      journal = journal_fixture()
      assert Content.get_journal!(journal.id) == journal
    end

    test "create_journal/1 with valid data creates a journal" do
      assert {:ok, %Journal{} = journal} = Content.create_journal(@valid_attrs)
      assert journal.completed_at == DateTime.from_naive!(~N[2010-04-17T14:00:00Z], "Etc/UTC")
      assert journal.description == "some description"
      assert journal.public == true
      assert journal.started_at == DateTime.from_naive!(~N[2010-04-17T14:00:00Z], "Etc/UTC")
      assert journal.tags == []
      assert journal.title == "some title"
    end

    test "create_journal/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Content.create_journal(@invalid_attrs)
    end

    test "update_journal/2 with valid data updates the journal" do
      journal = journal_fixture()
      assert {:ok, %Journal{} = journal} = Content.update_journal(journal, @update_attrs)
      assert journal.completed_at == DateTime.from_naive!(~N[2011-05-18T15:01:01Z], "Etc/UTC")
      assert journal.description == "some updated description"
      assert journal.public == false
      assert journal.started_at == DateTime.from_naive!(~N[2011-05-18T15:01:01Z], "Etc/UTC")
      assert journal.tags == []
      assert journal.title == "some updated title"
    end

    test "update_journal/2 with invalid data returns error changeset" do
      journal = journal_fixture()
      assert {:error, %Ecto.Changeset{}} = Content.update_journal(journal, @invalid_attrs)
      assert journal == Content.get_journal!(journal.id)
    end

    test "delete_journal/1 deletes the journal" do
      journal = journal_fixture()
      assert {:ok, %Journal{}} = Content.delete_journal(journal)
      assert_raise Ecto.NoResultsError, fn -> Content.get_journal!(journal.id) end
    end

    test "change_journal/1 returns a journal changeset" do
      journal = journal_fixture()
      assert %Ecto.Changeset{} = Content.change_journal(journal)
    end
  end

  describe "entries" do
    alias Standup.Content.Entry

    @valid_attrs %{body: "some body", public: true, title: "some title"}
    @update_attrs %{body: "some updated body", public: false, title: "some updated title"}
    @invalid_attrs %{body: nil, public: nil, title: nil}

    def entry_fixture(attrs \\ %{}) do
      {:ok, entry} =
        attrs
        |> Enum.into(@valid_attrs)
        |> Content.create_entry()

      entry
    end

    test "list_entries/0 returns all entries" do
      entry = entry_fixture()
      assert Content.list_entries() == [entry]
    end

    test "get_entry!/1 returns the entry with given id" do
      entry = entry_fixture()
      assert Content.get_entry!(entry.id) == entry
    end

    test "create_entry/1 with valid data creates a entry" do
      assert {:ok, %Entry{} = entry} = Content.create_entry(@valid_attrs)
      assert entry.body == "some body"
      assert entry.public == true
      assert entry.title == "some title"
    end

    test "create_entry/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Content.create_entry(@invalid_attrs)
    end

    test "update_entry/2 with valid data updates the entry" do
      entry = entry_fixture()
      assert {:ok, %Entry{} = entry} = Content.update_entry(entry, @update_attrs)
      assert entry.body == "some updated body"
      assert entry.public == false
      assert entry.title == "some updated title"
    end

    test "update_entry/2 with invalid data returns error changeset" do
      entry = entry_fixture()
      assert {:error, %Ecto.Changeset{}} = Content.update_entry(entry, @invalid_attrs)
      assert entry == Content.get_entry!(entry.id)
    end

    test "delete_entry/1 deletes the entry" do
      entry = entry_fixture()
      assert {:ok, %Entry{}} = Content.delete_entry(entry)
      assert_raise Ecto.NoResultsError, fn -> Content.get_entry!(entry.id) end
    end

    test "change_entry/1 returns a entry changeset" do
      entry = entry_fixture()
      assert %Ecto.Changeset{} = Content.change_entry(entry)
    end
  end
end
