defmodule Solutions.Day06 do
  alias Helpers.Readers, as: Reader

  def genChunker(len) do
    chunk_fun = fn element, acc ->
      if length(acc) == len do
        {:cont, Enum.reverse(acc), [element | Enum.drop(acc, -1)]}
      else
        {:cont, [element | acc]}
      end
    end
    after_fun = fn
      [] -> {:cont, []}
      acc -> {:cont, Enum.reverse(acc), []}
    end
    [chunk_fun, after_fun]
  end

  def getPacketStart(data, len) do
    [chunker, afterfun] = genChunker(len)
    Enum.map(data, &String.to_charlist/1) |> Enum.map(fn x-> Enum.chunk_while(x, [], chunker, afterfun) end) |>
      Enum.map(fn row ->
        Enum.map(row, fn entry ->
          MapSet.new(entry) |> Enum.count
        end)
      end)
    |> Enum.map(fn row ->
      Enum.find_index(row, fn x -> x == len end) + len
    end) |> List.first
  end

  def run(args) do
    data = Reader.readFile(args)
    getPacketStart(data, 4)
    |> IO.inspect(label: "Task 01")
    getPacketStart(data, 14)
    |> IO.inspect(label: "Task 02")
  end
end
