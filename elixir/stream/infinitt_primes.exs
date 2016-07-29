defmodule Prime do
  def integers() do
    Stream.concat([2], Stream.iterate(3, fn a -> a + 2 end))
  end
%ls
  def primes(s) do
    Stream.resource(
      fn -> s end,
      fn acc ->
        prime = Enum.take(acc, 1) |> hd
        {[prime], Stream.reject(acc, fn n -> rem(n, prime) == 0 end)}
      end,
      fn _ -> end
    )
  end
end

Prime.primes(Prime.integers) |> Enum.take(10)
