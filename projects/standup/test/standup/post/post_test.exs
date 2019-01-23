defmodule Standup.PostTest do
  use Standup.DataCase

  alias Standup.Post

  describe "stories" do
    alias Standup.Post.Story

    @valid_attrs %{description: "some description", title: "some title"}
    @update_attrs %{description: "some updated description", title: "some updated title"}
    @invalid_attrs %{description: nil, title: nil}

    def story_fixture(attrs \\ %{}) do
      {:ok, story} =
        attrs
        |> Enum.into(@valid_attrs)
        |> Post.create_story()

      story
    end

    test "list_stories/0 returns all stories" do
      story = story_fixture()
      assert Post.list_stories() == [story]
    end

    test "get_story!/1 returns the story with given id" do
      story = story_fixture()
      assert Post.get_story!(story.id) == story
    end

    test "create_story/1 with valid data creates a story" do
      assert {:ok, %Story{} = story} = Post.create_story(@valid_attrs)
      assert story.description == "some description"
      assert story.title == "some title"
    end

    test "create_story/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Post.create_story(@invalid_attrs)
    end

    test "update_story/2 with valid data updates the story" do
      story = story_fixture()
      assert {:ok, %Story{} = story} = Post.update_story(story, @update_attrs)
      assert story.description == "some updated description"
      assert story.title == "some updated title"
    end

    test "update_story/2 with invalid data returns error changeset" do
      story = story_fixture()
      assert {:error, %Ecto.Changeset{}} = Post.update_story(story, @invalid_attrs)
      assert story == Post.get_story!(story.id)
    end

    test "delete_story/1 deletes the story" do
      story = story_fixture()
      assert {:ok, %Story{}} = Post.delete_story(story)
      assert_raise Ecto.NoResultsError, fn -> Post.get_story!(story.id) end
    end

    test "change_story/1 returns a story changeset" do
      story = story_fixture()
      assert %Ecto.Changeset{} = Post.change_story(story)
    end
  end

  describe "updates" do
    alias Standup.Post.Update

    @valid_attrs %{body: "some body", title: "some title"}
    @update_attrs %{body: "some updated body", title: "some updated title"}
    @invalid_attrs %{body: nil, title: nil}

    def update_fixture(attrs \\ %{}) do
      {:ok, update} =
        attrs
        |> Enum.into(@valid_attrs)
        |> Post.create_update()

      update
    end

    test "list_updates/0 returns all updates" do
      update = update_fixture()
      assert Post.list_updates() == [update]
    end

    test "get_update!/1 returns the update with given id" do
      update = update_fixture()
      assert Post.get_update!(update.id) == update
    end

    test "create_update/1 with valid data creates a update" do
      assert {:ok, %Update{} = update} = Post.create_update(@valid_attrs)
      assert update.body == "some body"
      assert update.title == "some title"
    end

    test "create_update/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Post.create_update(@invalid_attrs)
    end

    test "update_update/2 with valid data updates the update" do
      update = update_fixture()
      assert {:ok, %Update{} = update} = Post.update_update(update, @update_attrs)
      assert update.body == "some updated body"
      assert update.title == "some updated title"
    end

    test "update_update/2 with invalid data returns error changeset" do
      update = update_fixture()
      assert {:error, %Ecto.Changeset{}} = Post.update_update(update, @invalid_attrs)
      assert update == Post.get_update!(update.id)
    end

    test "delete_update/1 deletes the update" do
      update = update_fixture()
      assert {:ok, %Update{}} = Post.delete_update(update)
      assert_raise Ecto.NoResultsError, fn -> Post.get_update!(update.id) end
    end

    test "change_update/1 returns a update changeset" do
      update = update_fixture()
      assert %Ecto.Changeset{} = Post.change_update(update)
    end
  end
end
