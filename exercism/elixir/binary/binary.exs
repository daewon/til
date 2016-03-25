defmodule Binary do
  @doc """
  Convert a string containing a binary number to an integer.

  On errors returns 0.
  """
  @spec to_decimal(String.t) :: non_neg_integer
  def to_decimal(string) do
    string
    |> to_valid_integer
    |> Enum.reverse
    |> Enum.with_index
    |> Enum.map(fn {n, m} -> map_to_radix(m, n) end)
    |> Enum.sum
  end

  defp to_valid_integer(str) do
    case str |> Integer.parse do
      {n, ""} -> n
      _ -> 0
    end

    |> Integer.digits
    |> Enum.filter(&(&1 == 0 or &1 == 1))
  end

  defp map_to_radix(_, 0), do: 0
  defp map_to_radix(m, n), do: n * (:math.pow(2, m) |> round)
end
