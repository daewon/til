defmodule Allergies do
  @allergies %{
    1 => "eggs",
    2 => "peanuts",
    4 => "shellfish",
    8 => "strawberries",
    16 => "tomatoes",
    32 => "chocolate",
    64 => "pollen",
    128 => "cats"
  } |> Enum.to_list

  @doc """
  List the allergies for which the corresponding flag bit is true.
  """
  @spec list(non_neg_integer) :: [String.t]
  def list(flags) do
    nums = @allergies
    |> Enum.reject(fn {k, _} -> k > flags end)
    |> Enum.reverse

    find(nums, flags)
  end

  def find([], _), do: []
  def find([{n, a}|t], flags) when flags-n > 0, do: [a | list(flags-n)]
  def find([{_, a}|_], _), do: [a]

  @doc """
  Returns whether the corresponding flag bit in 'flags' is set for the item.
  """
  @spec allergic_to?(non_neg_integer, String.t) :: boolean
  def allergic_to?(flags, item), do: item in list(flags)
end
