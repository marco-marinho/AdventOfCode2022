defmodule Solutions.Day03 do
  alias Helpers.Readers, as: Reader

  def split(input) do
    len = String.length(input)
    input
    |> String.to_charlist
    |> Enum.chunk_every(div(len, 2))
  end

  def findRepeat(input) do
    [first, second] = input
    MapSet.intersection(MapSet.new(first), MapSet.new(second)) |> Enum.take(1) |> List.first
  end

  def findBadge(input) do
    [first, second, third] = Enum.map(input, &String.to_charlist/1) |> Enum.map(&MapSet.new/1)
    MapSet.intersection(third, MapSet.intersection(first, second)) |> Enum.take(1) |> List.first |> calcPriority
  end

  def calcPriority(input) do
    cond do
      input > 90 -> input - 96
      true -> input - 64 + 26
    end
  end

  def run(args) do
    data = Reader.readFile(args)
    Enum.map(data, &split/1) |> Enum.map(&findRepeat/1) |> Enum.sum |> IO.inspect(label: "Task 01")
    Enum.chunk_every(data, 3) |> Enum.map(&findBadge/1) |> Enum.sum |> IO.inspect(label: "Task 02")
  end
end
