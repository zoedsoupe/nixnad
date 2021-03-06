defmodule DotfilesTest do
  use ExUnit.Case
  doctest Dotfiles

  test "greets the world" do
    assert Dotfiles.hello() == :world
  end
end
