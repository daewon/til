defmodule Isogram do
  @doc """
  Determines if a word or sentence is an isogram
  """
  @spec isogram?(String.t) :: boolean
  def isogram?(sentence) do
    chars = sentence
    |> String.downcase
    |> String.graphemes
    |> Enum.filter(fn c -> c != "-" && c != " " end)
    |> Enum.map(fn c -> %{c => 1} end)

    counted = chars
    |> Enum.reduce(%{}, fn wm, acc ->
      Map.merge(acc, wm, fn _k, v1, v2 -> v1 + v2 end)
    end)

    counted
    |> Enum.all?(fn {k, v} -> v == 1 end)
  end

end
