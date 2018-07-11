defmodule Vitalsigns.WebTest do
  use Vitalsigns.DataCase

  alias Vitalsigns.Web

  describe "users" do
    alias Vitalsigns.Web.User

    @valid_attrs %{email: "some email", name: "some name", pw_hash: "some pw_hash"}
    @update_attrs %{email: "some updated email", name: "some updated name", pw_hash: "some updated pw_hash"}
    @invalid_attrs %{email: nil, name: nil, pw_hash: nil}

    def user_fixture(attrs \\ %{}) do
      {:ok, user} =
        attrs
        |> Enum.into(@valid_attrs)
        |> Web.create_user()

      user
    end

    test "list_users/0 returns all users" do
      user = user_fixture()
      assert Web.list_users() == [user]
    end

    test "get_user!/1 returns the user with given id" do
      user = user_fixture()
      assert Web.get_user!(user.id) == user
    end

    test "create_user/1 with valid data creates a user" do
      assert {:ok, %User{} = user} = Web.create_user(@valid_attrs)
      assert user.email == "some email"
      assert user.name == "some name"
      assert user.pw_hash == "some pw_hash"
    end

    test "create_user/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Web.create_user(@invalid_attrs)
    end

    test "update_user/2 with valid data updates the user" do
      user = user_fixture()
      assert {:ok, user} = Web.update_user(user, @update_attrs)
      assert %User{} = user
      assert user.email == "some updated email"
      assert user.name == "some updated name"
      assert user.pw_hash == "some updated pw_hash"
    end

    test "update_user/2 with invalid data returns error changeset" do
      user = user_fixture()
      assert {:error, %Ecto.Changeset{}} = Web.update_user(user, @invalid_attrs)
      assert user == Web.get_user!(user.id)
    end

    test "delete_user/1 deletes the user" do
      user = user_fixture()
      assert {:ok, %User{}} = Web.delete_user(user)
      assert_raise Ecto.NoResultsError, fn -> Web.get_user!(user.id) end
    end

    test "change_user/1 returns a user changeset" do
      user = user_fixture()
      assert %Ecto.Changeset{} = Web.change_user(user)
    end
  end

  describe "posts" do
    alias Vitalsigns.Web.Post

    @valid_attrs %{body: "some body", created_at: ~N[2010-04-17 14:00:00.000000], deleted_at: ~N[2010-04-17 14:00:00.000000], location: "some location", title: "some title", updated_at: ~N[2010-04-17 14:00:00.000000]}
    @update_attrs %{body: "some updated body", created_at: ~N[2011-05-18 15:01:01.000000], deleted_at: ~N[2011-05-18 15:01:01.000000], location: "some updated location", title: "some updated title", updated_at: ~N[2011-05-18 15:01:01.000000]}
    @invalid_attrs %{body: nil, created_at: nil, deleted_at: nil, location: nil, title: nil, updated_at: nil}

    def post_fixture(attrs \\ %{}) do
      {:ok, post} =
        attrs
        |> Enum.into(@valid_attrs)
        |> Web.create_post()

      post
    end

    test "list_posts/0 returns all posts" do
      post = post_fixture()
      assert Web.list_posts() == [post]
    end

    test "get_post!/1 returns the post with given id" do
      post = post_fixture()
      assert Web.get_post!(post.id) == post
    end

    test "create_post/1 with valid data creates a post" do
      assert {:ok, %Post{} = post} = Web.create_post(@valid_attrs)
      assert post.body == "some body"
      assert post.created_at == ~N[2010-04-17 14:00:00.000000]
      assert post.deleted_at == ~N[2010-04-17 14:00:00.000000]
      assert post.location == "some location"
      assert post.title == "some title"
      assert post.updated_at == ~N[2010-04-17 14:00:00.000000]
    end

    test "create_post/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Web.create_post(@invalid_attrs)
    end

    test "update_post/2 with valid data updates the post" do
      post = post_fixture()
      assert {:ok, post} = Web.update_post(post, @update_attrs)
      assert %Post{} = post
      assert post.body == "some updated body"
      assert post.created_at == ~N[2011-05-18 15:01:01.000000]
      assert post.deleted_at == ~N[2011-05-18 15:01:01.000000]
      assert post.location == "some updated location"
      assert post.title == "some updated title"
      assert post.updated_at == ~N[2011-05-18 15:01:01.000000]
    end

    test "update_post/2 with invalid data returns error changeset" do
      post = post_fixture()
      assert {:error, %Ecto.Changeset{}} = Web.update_post(post, @invalid_attrs)
      assert post == Web.get_post!(post.id)
    end

    test "delete_post/1 deletes the post" do
      post = post_fixture()
      assert {:ok, %Post{}} = Web.delete_post(post)
      assert_raise Ecto.NoResultsError, fn -> Web.get_post!(post.id) end
    end

    test "change_post/1 returns a post changeset" do
      post = post_fixture()
      assert %Ecto.Changeset{} = Web.change_post(post)
    end
  end

  describe "attachments" do
    alias Vitalsigns.Web.Attachment

    @valid_attrs %{body: "some body", caption: "some caption", kind: "some kind", url: "some url"}
    @update_attrs %{body: "some updated body", caption: "some updated caption", kind: "some updated kind", url: "some updated url"}
    @invalid_attrs %{body: nil, caption: nil, kind: nil, url: nil}

    def attachment_fixture(attrs \\ %{}) do
      {:ok, attachment} =
        attrs
        |> Enum.into(@valid_attrs)
        |> Web.create_attachment()

      attachment
    end

    test "list_attachments/0 returns all attachments" do
      attachment = attachment_fixture()
      assert Web.list_attachments() == [attachment]
    end

    test "get_attachment!/1 returns the attachment with given id" do
      attachment = attachment_fixture()
      assert Web.get_attachment!(attachment.id) == attachment
    end

    test "create_attachment/1 with valid data creates a attachment" do
      assert {:ok, %Attachment{} = attachment} = Web.create_attachment(@valid_attrs)
      assert attachment.body == "some body"
      assert attachment.caption == "some caption"
      assert attachment.kind == "some kind"
      assert attachment.url == "some url"
    end

    test "create_attachment/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Web.create_attachment(@invalid_attrs)
    end

    test "update_attachment/2 with valid data updates the attachment" do
      attachment = attachment_fixture()
      assert {:ok, attachment} = Web.update_attachment(attachment, @update_attrs)
      assert %Attachment{} = attachment
      assert attachment.body == "some updated body"
      assert attachment.caption == "some updated caption"
      assert attachment.kind == "some updated kind"
      assert attachment.url == "some updated url"
    end

    test "update_attachment/2 with invalid data returns error changeset" do
      attachment = attachment_fixture()
      assert {:error, %Ecto.Changeset{}} = Web.update_attachment(attachment, @invalid_attrs)
      assert attachment == Web.get_attachment!(attachment.id)
    end

    test "delete_attachment/1 deletes the attachment" do
      attachment = attachment_fixture()
      assert {:ok, %Attachment{}} = Web.delete_attachment(attachment)
      assert_raise Ecto.NoResultsError, fn -> Web.get_attachment!(attachment.id) end
    end

    test "change_attachment/1 returns a attachment changeset" do
      attachment = attachment_fixture()
      assert %Ecto.Changeset{} = Web.change_attachment(attachment)
    end
  end
end
