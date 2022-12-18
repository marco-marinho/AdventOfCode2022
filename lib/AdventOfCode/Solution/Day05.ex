defmodule Solutions.Day05 do
  alias Helpers.Readers, as: Reader

  def getRow(input) do
    input |> String.graphemes() |> Enum.drop(1) |> Enum.take_every(4) |> Enum.with_index(1)
  end

  def runCommand(input, stack) do
    [times, source, dest] = input

    1..times
    |> Enum.reduce(stack, fn _, curstack ->
      [crate | remaining] = Map.get(curstack, source)
      drop = Map.get(curstack, dest)
      Map.put(curstack, source, remaining) |> Map.put(dest, [crate | drop])
    end)
  end

  def runCommand9001(input, stack) do
    [times, source, dest] = input
    take = Enum.take(Map.get(stack, source), times)
    remaining = Enum.drop(Map.get(stack, source), times)

    Enum.reverse(take)
    |> Enum.reduce(stack, fn el, curstack ->
      drop = Map.get(curstack, dest)
      Map.put(curstack, dest, [el | drop])
    end)
    |> Map.put(source, remaining)
  end

  def run(args) do
    data = Reader.readFile(args)
    split = Reader.splitAtEmptyLines(data)

    placement =
      Enum.at(split, 0)
      |> Enum.drop(1)
      |> Enum.flat_map(&getRow/1)
      |> Enum.group_by(&elem(&1, 1))
      |> Enum.map(fn {column, vals} ->
        {column,
         vals |> Enum.map(&elem(&1, 0)) |> Enum.reject(&Kernel.==(&1, " ")) |> Enum.reverse()}
      end)
      |> Map.new()

    commands =
      Enum.at(split, 1)
      |> Enum.map(fn x ->
        String.replace(x, "move ", "")
        |> String.replace(" from ", ",")
        |> String.replace(" to ", ",")
        |> String.split(",")
        |> Enum.map(fn y ->
          String.to_integer(y)
        end)
      end)
      |> Enum.reverse()

    res =
      commands
      |> Enum.reduce(placement, fn command, currstack ->
        runCommand(command, currstack)
      end)

    res2 =
      commands
      |> Enum.reduce(placement, fn command, currstack ->
        runCommand9001(command, currstack)
      end)

    1..Enum.count(res)
    |> Enum.reduce("", fn key, curr ->
      curr <> List.first(Map.get(res, key))
    end)
    |> IO.inspect(label: "Task 01")

    1..Enum.count(res2)
    |> Enum.reduce("", fn key, curr ->
      curr <> List.first(Map.get(res2, key))
    end)
    |> IO.inspect(label: "Task 02")
  end
end
