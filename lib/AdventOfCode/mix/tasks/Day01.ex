defmodule Mix.Tasks.Day01 do
  use Mix.Task

  def run(_args) do
    Solutions.Day01.run("Data/Day01.txt")
  end
end
