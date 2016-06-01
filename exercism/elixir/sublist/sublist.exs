defmodule Sublist do
  @doc """
  Returns whether the first list is a sub_list or a superlist of the second list
  and if not whether it is equal or unequal to the second list.
  """
  def compare(a, b) do
    case compare_length(a, b) do
      :eq -> if sub_list?(a, b), do: :equal, else: :unequal
      :gt -> if sub_list?(b, a), do: :superlist, else: :unequal
      :lt -> if sub_list?(a, b), do: :sublist, else: :unequal
    end
  end

  defp compare_length(a, b) do
    find = fn
      {[], []}, _ -> :eq
      {_, []}, _ -> :gt
      {[], _}, _ -> :lt
      {[_|at], [_|bt]}, f -> f.({at, bt}, f)
    end

    find.({a, b}, find)
  end

  def sub_list?([], _), do: true
  def sub_list?(_, []), do: false
  def sub_list?(a, [_ | bt] = b), do: starts_with?(a, b) || sub_list?(a, bt)

  defp starts_with?([], _), do: true
  defp starts_with?(_, []), do: false
  defp starts_with?([ah| at], [bh| bt]) do
    case ah === bh do
      true -> starts_with?(at, bt)
      false -> false
    end
  end
end
