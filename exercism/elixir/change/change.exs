defmodule Change do
  @doc """
  Determine the least number of coins to be given to the user such
  that the sum of the coins' value would equal the correct amount of change.
  It returns :error if it is not possible to compute the right amount of coins.
  Otherwise returns the tuple {:ok, map_of_coins}

  ## Examples

  iex> Change.generate(3, [5, 10, 15])
  :error

  iex> Change.generate(18, [1, 5, 10])
  {:ok, %{1 => 3, 5 => 1, 10 => 1}}

  """

  @spec generate(integer, list) :: {:ok, map} | :error
  def generate(amount, values) do
    base = Enum.map(values, &({&1, 0})) |> Enum.into(%{})

    coins = do_change(amount, values, amount, [])
    |> Enum.group_by(&(&1))
    |> Enum.map(fn {k, coins} -> {k, length(coins)} end)

    format_result(base, coins)
  end

  @spec format_result(map, list) :: {:ok, map} | {:error}
  def format_result(_, []), do: :error
  def format_result(base, coins), do: {:ok, Enum.into(coins, base)}

  @spec do_change(integer, list, integer, list) :: [integer]
  defp do_change(_, [], _, _), do: []
  defp do_change(_, _, remain, _) when remain < 0, do: []
  defp do_change(_, _, remain, acc) when remain == 0, do: acc
  defp do_change(amount, [h|t]=coins, remain, acc) do
    used = do_change(amount, coins, remain - h, [h|acc])
    not_used = do_change(amount, t, remain, acc)

    case {used, not_used} do
      {[], _} -> not_used
      {_, []} -> used
      _ -> if length(used) < length(not_used), do: used, else: not_used
    end
  end
end
