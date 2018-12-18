defmodule BoomTest do
  use ExUnit.Case
  doctest Boom

  test "greets the world" do
    assert Boom.hello() == :world
  end
end
