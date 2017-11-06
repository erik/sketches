# TODO: Periodically run through members, purge timeouts...

defmodule Pontoon.Membership do
  require Logger
  use Agent

  def start_link(_opts \\ []) do
    Logger.info("initializing membership list")

    {:ok, pid} = Agent.start_link(fn -> MapSet.new() end, name: __MODULE__)
  end

  def add_member(member) do
    Agent.update(__MODULE__, fn state ->
      # TODO: Update state and stuff
      state
    end)
  end

  def remove_member(member) do
    Agent.update(__MODULE__, fn state ->
      # TODO: update the state and stuff
    end)
  end
end
