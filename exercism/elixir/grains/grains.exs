defmodule Grains do
  @doc """
  Calculate two to the power of the input minus one.
  """
  @spec square(pos_integer) :: pos_integer
  def square(0), do: 1
  def square(number) do
    Stream.cycle([2])
    |> Enum.take(number-1)
    |> Enum.reduce(1, fn (n, acc) -> acc * n end)
  end

  @doc """
  Adds square of each number from 1 to 64.
  """
  @spec total :: pos_integer
  def total do
    1..64 |> Enum.map(&square/1) |> Enum.sum
  end
end
