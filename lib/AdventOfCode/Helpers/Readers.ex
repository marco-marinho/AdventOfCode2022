defmodule Helpers.Readers do

  def readFile(path) do
    {:ok, contents} = File.read(path)
    contents |> String.replace("\r", "") |> String.split("\n")
  end

end
