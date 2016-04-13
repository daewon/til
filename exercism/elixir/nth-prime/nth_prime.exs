defmodule Prime do
  @doc """
  Generates the nth prime.
  """
  @spec nth(non_neg_integer) :: non_neg_integer
  def nth(0), do: raise ArgumentError
  def nth(count) do
    Stream.concat([2], Stream.iterate(3, &(&1 + 2)))
  end

  defp prime?(2), do: true
  defp prime?(n) do
    nums = 2..(:math.sqrt(n) |> Kernel.round)
    |> Stream.filter(fn m -> rem(n, m) == 0 end)
    |> Enum.take(1)

    length(nums) == 0
  end
end
