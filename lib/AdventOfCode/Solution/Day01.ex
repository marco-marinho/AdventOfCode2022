defmodule Solutions.Day01 do
  alias Helpers.Readers, as: Reader

  def run(args) do
    data = Reader.readFile(args)

    chunk_fun = fn element, acc ->
      if String.length(element) == 0 do
        {:cont, Enum.sum(acc), []}
      else
        {:cont, [elem(Integer.parse(element), 0) | acc]}
      end
    end

    after_fun = fn
      [] -> {:cont, []}
      acc -> {:cont, Enum.sum(acc), []}
    end

    data = Enum.chunk_while(data, [], chunk_fun, after_fun)

    t1 = data
      |> Enum.max()

    t2 =
      data
      |> Enum.sort()
      |> Enum.take(-3)
      |> Enum.sum()

    IO.puts("Day 01: #{t1}")
    IO.puts("Day 02: #{t2}")
  end
end
