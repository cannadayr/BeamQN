defmodule BeamQNTest do
  use ExUnit.Case
  doctest BeamQN

  test "greets the world" do
    assert BeamQN.hello() == :world
  end
end
