defmodule PontoonTest do
  use ExUnit.Case
  doctest Pontoon

  test "greets the world" do
    assert Pontoon.hello() == :world
  end
end
