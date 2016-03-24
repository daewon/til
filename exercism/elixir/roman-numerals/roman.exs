defmodule Roman do
  @ls [{1000, "M"},
       {900, "CM"},
       {500, "D"},
       {400, "CD"},
       {100, "C"},
       {90, "XC"},
       {80, "LXXX"},
       {70, "LXX"},
       {60, "LX"},
       {50, "L"},
       {40, "XL"},
       {10, "X"},
       {9, "IX"},
       {5, "V"},
       {4, "IV"},
       {1, "I"}]

  @keys @ls |> Enum.map(fn {k, _} -> k end)
  @map @ls |> Enum.into(%{})

  @doc """
  Convert the number to a roman number.
  """
  @spec numerals(pos_integer) :: String.t
  def numerals(number) do
    calc(@keys, number) |> Enum.join("")
  end

  defp calc([], _), do: []
  defp calc([k|t]=ls, number) do
    cond do
      number - k >= 0 -> [@map[k] | calc(ls, number-k)]
      true -> calc(t, number)
    end
  end
end
