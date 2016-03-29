defmodule Series do

  @doc """
  Finds the largest product of a given number of consecutive numbers in a given string of numbers.
  """
  @spec largest_product(String.t, non_neg_integer) :: non_neg_integer

  def largest_product(number_string, 0), do: 1
  def largest_product(number_string, size) do
    digits = number_string |> String.to_integer |> Integer.digits
    do_largest_product(digits, size)
  end

  defp do_largest_product([0], size), do: 0
  defp do_largest_product(_, size) when size < 0, do: raise ArgumentError
  defp do_largest_product(d, s) when length(d) < s, do: raise ArgumentError
  defp do_largest_product(digits, size) do
    digits
    |> Enum.chunk(size, 1)
    |> Enum.reduce(0, fn ls, acc ->
      max(Enum.reduce(ls, 1, &Kernel.*/2), acc)
    end)
  end
end
