defmodule Palindromes do

  @doc """
  Generates all palindrome products from an optionally given min factor (or 1) to a given max factor.
  """
  @spec generate(non_neg_integer, non_neg_integer) :: map
  def generate(max, min \\ 1) do
    products = for a <- min..max, b <- a..max,
      product = a * b,
      str = Integer.to_string(product),
      rev = String.reverse(str),
      str == rev, do: %{product => [[a, b]]}

    Enum.reduce(products, %{}, fn m, acc ->
      Map.merge(acc, m, fn _k, o, n -> o ++ n end)
    end)
  end
end
