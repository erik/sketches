defmodule Pronk.ServerConnection do
  use Agent, restart: :temporary
end
