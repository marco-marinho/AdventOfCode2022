defmodule Mix.Tasks.Day04 do
  use Mix.Task

  def run(_args) do
    Solutions.Day04.run("Data/Day04.txt")
  end
end