defmodule Sieve do
  @doc """
  Generates a list of primes up to a given limit.
  """
  @spec primes_to(non_neg_integer) :: [non_neg_integer]
  def primes_to(limit) do
    primes |> Enum.take(10)
  end

  # primes  = sieve [2..]
  #           where
  #           sieve
  # (p:xs) = p : sieve [x | x <- xs, rem x p > 0]

  defp primes(prime, n) do
    Stream.concat(2..2, Stream.filter(is_prime, primes(3..5)))
  end

  defp is_prime(primes, n) do
    {p, ps} = {Stream.take(1, primes), Stream.drop(1, primes)}
    p*p > n || rem(n, p) !== 0 && is_prime(ps, n)
  end
end
