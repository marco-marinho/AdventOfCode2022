defmodule Solutions.Day04 do
  alias Helpers.Readers, as: Reader

  def checkRedundant(input) do
    [min1, max1, min2, max2] = input
    cond do
      (min1 <= min2 && max1 >= max2) || (min2 <= min1 && max2 >= max1) -> 1
      true -> 0
    end
  end

  def checkRedundantT2(input) do
    [min1, max1, min2, max2] = input
    cond do
      (max1 >= min2 && max1 <= max2) || (max2 >= min1 && max2 <= max1) -> 1
      true -> 0
    end
  end

  def run(args) do
      data = Reader.readFile(args)
      |> Enum.map(fn x -> String.replace(x, "-", ",") end)
      |> Enum.map(fn x -> String.split(x, ",") end)
      |> Enum.map(fn x -> Enum.map(x, &String.to_integer/1) end)

      Enum.map(data, &checkRedundant/1)
      |> Enum.sum
      |> IO.inspect(label: "Task 01")

      Enum.map(data, &checkRedundantT2/1)
      |> Enum.sum
      |> IO.inspect(label: "Task 02")
  end
end
