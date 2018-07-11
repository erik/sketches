defmodule Vitalsigns.Web.Attachment do
  use Ecto.Schema
  import Ecto.Changeset


  schema "attachments" do
    field :body, :string
    field :caption, :string
    field :kind, :string
    field :url, :string
    field :post_id, :id

    timestamps()
  end

  @doc false
  def changeset(attachment, attrs) do
    attachment
    |> cast(attrs, [:caption, :kind, :url, :body])
    |> validate_required([:caption, :kind, :url, :body])
  end
end
