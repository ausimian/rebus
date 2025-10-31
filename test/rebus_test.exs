defmodule RebusTest do
  use ExUnit.Case
  doctest Rebus

  test "greets the world" do
    assert Rebus.hello() == :world
  end
end
