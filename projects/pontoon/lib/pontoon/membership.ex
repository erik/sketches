defmodule Pontoon.Membership do
  require Logger
  use Agent

  def start_link(_opts \\ []) do
    Logger.info("initializing membership list")

    Agent.start_link(fn -> MapSet.new() end)
  end

  def receive_broadcast(member) do
  end
end
