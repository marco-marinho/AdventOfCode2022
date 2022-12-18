defmodule Mix.Tasks.Day03 do
  use Mix.Task

  def run(_args) do
    Solutions.Day03.run("Data/Day03.txt")
  end
end