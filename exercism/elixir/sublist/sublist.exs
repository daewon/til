defmodule Sublist do
  @doc """
  Returns whether the first list is a sub_list or a superlist of the second list
  and if not whether it is equal or unequal to the second list.
  """
  def compare(a, b) do
    a_len = length(a)
    b_len = length(b)

    cond do
      a_len === b_len && equal_list?(a, b) -> :equal
      a_len > b_len && sub_list?(b, a) -> :superlist
      a_len < b_len && sub_list?(a, b) -> :sublist
      true -> :unequal
    end
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

  def equal_list?([], []), do: true
  def equal_list?([ah | t], [bh | bt]) do
    case ah === bh do
      true -> equal_list?(t, bt)
      false -> false
    end
  end
end
