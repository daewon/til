defmodule Bowling do
  defstruct frames: []

  @doc """
  Creates a new game of bowling that can be used to store the results of
  the game
  """

  @spec start() :: any
  def start do
    %Bowling{}
  end

  @doc """
  Records the number of pins knocked down on a single roll. Returns `:ok`
  unless there is something wrong with the given number of pins, in which
  case it returns a helpful message.
  """

  @spec roll(any, integer) :: any | String.t
  def roll(%Bowling{frames: frames}, roll), do: %Bowling{frames: frames ++ [roll]}
  @doc """
  Returns the score of a given game of bowling if the game is complete.
  If the game isn't complete, it returns a helpful message.
  """
  @spec score(any) :: integer | String.t
  def score(%Bowling{frames: frames}) do
    games = frames
    |> Enum.flat_map(fn
      10 -> [10, nil]
      a -> [a]
    end)
    |> Enum.chunk(2, 2, [])

    {fst, snd} = Enum.split(games, 9)
    {normal_game, rest_game} = {flat_game(fst), flat_game(snd)}

    loop(normal_game, rest_game, 0) + Enum.sum(rest_game)
  end

  defp flat_game(ls), do: ls |> List.flatten |> Enum.reject(&(&1 == nil))

  defp sum(ls, rest, n) do
    Stream.flat_map([ls, rest], fn (a) -> a end)
    |> Enum.take(n)
    |> Enum.sum
  end

  defp loop([], _, acc), do: acc
  defp loop([10|tl], rest, acc), do: loop(tl, rest, acc + 10 + sum(tl, rest, 2))
  defp loop([a, b|tl], rest, acc) do
    cond do
      a + b == 10 -> loop(tl, rest, acc + 10 + sum(tl, rest, 1))
      true -> loop(tl, rest, acc + a + b)
    end
  end
end
