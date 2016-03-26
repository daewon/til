defmodule PrimeFactors do
  @doc """
  Compute the prime factors for 'number'.

  The prime factors are prime numbers that when multiplied give the desired
  number.

  The prime factors of 'number' will be ordered lowest to highest.
  """
  @spec factors_for(pos_integer) :: [pos_integer]
  def factors_for(1), do: []
  def factors_for(m), do: factor_for(m, 2, []) |> Enum.reverse
  defp factor_for(m, m, acc), do: [m | acc]
  defp factor_for(m, n, acc) when rem(m, n) == 0, do: factor_for(div(m, n), n, [n | acc])
  defp factor_for(m, n, acc), do:  factor_for(m, n+1, acc)
end
