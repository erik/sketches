defmodule StandupWeb.Guardian do
  use Guardian, otp_app: :standup

  alias Standup.Accounts

  def subject_for_token(%{"id" => id}, _claims) do
    {:ok, to_string(id)}
  end

  def subject_for_token(_, _) do
    {:error, :bad_subject}
  end

  def resource_from_claims(%{"sub" => user_id}) do
    case Accounts.get_user(user_id) do
      nil ->
        {:error, :not_found}

      user ->
        {:ok, user}
    end
  end
end
