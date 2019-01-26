defmodule Standup.SocialTest do
  use Standup.DataCase

  alias Standup.Social

  describe "replies" do
    alias Standup.Social.Reply

    @valid_attrs %{body: "some body"}
    @update_attrs %{body: "some updated body"}
    @invalid_attrs %{body: nil}

    def reply_fixture(attrs \\ %{}) do
      {:ok, reply} =
        attrs
        |> Enum.into(@valid_attrs)
        |> Social.create_reply()

      reply
    end

    test "list_replies/0 returns all replies" do
      reply = reply_fixture()
      assert Social.list_replies() == [reply]
    end

    test "get_reply!/1 returns the reply with given id" do
      reply = reply_fixture()
      assert Social.get_reply!(reply.id) == reply
    end

    test "create_reply/1 with valid data creates a reply" do
      assert {:ok, %Reply{} = reply} = Social.create_reply(@valid_attrs)
      assert reply.body == "some body"
    end

    test "create_reply/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Social.create_reply(@invalid_attrs)
    end

    test "update_reply/2 with valid data updates the reply" do
      reply = reply_fixture()
      assert {:ok, %Reply{} = reply} = Social.update_reply(reply, @update_attrs)
      assert reply.body == "some updated body"
    end

    test "update_reply/2 with invalid data returns error changeset" do
      reply = reply_fixture()
      assert {:error, %Ecto.Changeset{}} = Social.update_reply(reply, @invalid_attrs)
      assert reply == Social.get_reply!(reply.id)
    end

    test "delete_reply/1 deletes the reply" do
      reply = reply_fixture()
      assert {:ok, %Reply{}} = Social.delete_reply(reply)
      assert_raise Ecto.NoResultsError, fn -> Social.get_reply!(reply.id) end
    end

    test "change_reply/1 returns a reply changeset" do
      reply = reply_fixture()
      assert %Ecto.Changeset{} = Social.change_reply(reply)
    end
  end
end
