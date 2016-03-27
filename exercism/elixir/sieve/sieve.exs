defmodule Sieve do
  @doc """
  Generates a list of primes up to a given limit.
  """
  @spec primes_to(non_neg_integer) :: [non_neg_integer]
  def primes_to(limit) do
    sieve(2, limit, [], %{}) |> Enum.reverse
  end

  defp sieve(_, limit, [p|ps], _) when p > limit, do: ps
  defp sieve(n, limit, acc, sieve_m) do
    filtered = Map.has_key?(sieve_m, n)
    cond do
      filtered -> sieve(n+1, limit, acc, sieve_m)
      prime?(n) -> sieve(n+1, limit, [n|acc], mark_sieve(sieve_m, n, limit))
      true -> sieve(n+1, limit, acc, sieve_m)
    end
  end

  defp mark_sieve(sieve_m, n, limit) do
    Stream.iterate(n, fn m -> m * 2 end)
    |> Stream.take_while(&(&1 <= limit))
    |> Enum.with_index
    |> Enum.into(sieve_m)
  end

  defp prime?(2), do: true
  defp prime?(n) do
    nums = 2..(:math.sqrt(n) |> Kernel.round)
    |> Stream.filter(fn m -> rem(n, m) == 0 end)
    |> Enum.take(1)

    length(nums) == 0
  end
end
