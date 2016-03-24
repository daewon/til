defmodule Roman do
  @nums [1000, 900, 500, 400, 100, 90, 80, 70, 60, 50, 40, 10, 9, 5, 4, 1]
  @numerals ["M", "CM", "D", "CD", "C", "XC", "LXXX", "LXX", "LX", "L", "XL", "X", "IX", "V", "IV", "I"]
  @map Enum.zip(@nums, @numerals) |> Enum.into(%{})

  @doc """
  Convert the number to a roman number.
  """
  @spec numerals(pos_integer) :: String.t
  def numerals(n), do: numerals(@nums, n) |> Enum.join("")
  defp numerals([], _), do: []
  defp numerals([h|t]=ls, n) when n - h >= 0, do: [@map[h] | numerals(ls, n-h)]
  defp numerals([_|t], n), do: numerals(t, n)
end
