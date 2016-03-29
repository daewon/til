defmodule SumOfMultiples do
  @doc """
  Adds up all numbers from 1 to a given end number that are multiples of the factors provided.
  """
  @spec to(non_neg_integer, [non_neg_integer]) :: non_neg_integer
  def to(limit, factors) do
    factors
    |> Enum.flat_map(fn n ->
      Stream.iterate(n, &( &1 + n) )
      |> Enum.take_while(&( &1 < limit ))
    end)
    |> Enum.uniq
    |> Enum.sum
  end
end
