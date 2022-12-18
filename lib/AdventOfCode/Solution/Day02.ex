defmodule Solutions.Day02 do
  alias Helpers.Readers, as: Reader

  def resultScore(adversary, yours) do
    dif = adversary - yours

    case dif do
      n when n in [1, -2] -> 0
      n when n in [-1, 2] -> 6
      0 -> 3
    end
  end

  def choiceScore(yours) do
    yours - 64
  end

  def getChoice(adversary, res) do
    case res do
      :draw -> adversary
      :win -> if adversary + 1 > 67, do: 65, else: adversary + 1
      :lose -> if adversary - 1 > 64, do: adversary - 1, else: 67
    end
  end

  def doRound(input) do
    [adversary, _, yours] = String.to_charlist(input)
    playMap = %{88 => 65, 89 => 66, 90 => 67}
    resultScore(adversary, playMap[yours]) + choiceScore(playMap[yours])
  end

  def doRoundT2(input) do
    [adversary, _, yours] = String.to_charlist(input)
    playMap = %{88 => :lose, 89 => :draw, 90 => :win}
    res = playMap[yours]
    choice = getChoice(adversary, res) |> choiceScore()
    case res do
      :lose -> choice
      :draw -> choice + 3
      :win -> choice + 6
    end
  end

  def run(args) do
    data = Reader.readFile(args)
    Enum.map(data, &doRound/1) |> Enum.sum() |> IO.inspect(label: "Task 01")
    Enum.map(data, &doRoundT2/1) |> Enum.sum() |> IO.inspect(label: "Task 02")
  end
end
