defmodule Helpers.Readers do

  def readFile(path) do
    {:ok, contents} = File.read(path)
    contents |> String.replace("\r", "") |> String.split("\n")
  end

  def splitAtEmptyLines(input) do
    chunk_fun = fn element, acc ->
      if String.length(element) == 0 do
        {:cont, acc, []}
      else
        {:cont, [element | acc]}
      end
    end

    after_fun = fn
      [] -> {:cont, []}
      acc -> {:cont, acc, []}
    end

    Enum.chunk_while(input, [], chunk_fun, after_fun)
  end

end
