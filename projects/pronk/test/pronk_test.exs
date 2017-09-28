defmodule PronkTest do
  use ExUnit.Case
  doctest Pronk

  test "greets the world" do
    assert Pronk.hello() == :world
  end
end
