defmodule Prime do
  @doc """
  Generates the nth prime.
  """
  @spec nth(non_neg_integer) :: non_neg_integer
  def nth(0), do: raise ArgumentError
  def nth(count) do
    primes
    |> Stream.drop(count-1)
    |> Enum.take(1)
    |> hd
  end

  def primes do
    initial = fn -> Stream.concat([2], Stream.iterate(3, fn a -> a + 2 end)) end
    next = fn acc ->
      prime = Enum.take(acc, 1) |> hd
      {[prime], Stream.reject(acc, fn n -> rem(n, prime) == 0 end)}
    end

    Stream.resource(initial, next, fn _ -> end)
  end
end
