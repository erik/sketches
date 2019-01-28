defmodule StandupWeb.Guardian do
  use Guardian, otp_app: :standup

  alias Standup.Accounts

  @magic "magic"
  @access "access"

  def subject_for_token(user, _claims) do
    {:ok, to_string(user.id)}
  end

  def subject_for_token(_, _) do
    {:error, :bad_subject}
  end

  def resource_from_claims(%{"sub" => user_id}) do
    case Accounts.get_user!(user_id) do
      nil ->
        {:error, :not_found}

      user ->
        {:ok, user}
    end
  end

  def encode_magic(resource, claims \\ %{}) do
    encode_and_sign(resource, claims, token_type: @magic)
  end

  def decode_magic(magic_token, claims \\ %{}) do
    resource_from_token(magic_token, claims, token_type: @magic)
  end

  def encode_access(resource, claims \\ %{}) do
    encode_and_sign(resource, claims, token_type: @access)
  end

  def decode_access(access_token, claims \\ %{}) do
    resource_from_token(access_token, claims, token_type: @access)
  end

  def exchange_magic(magic_token) do
    with {:ok, _, {token, claims}} <- exchange(magic_token, @magic, @access) do
      {:ok, token, claims}
    end
  end
end
