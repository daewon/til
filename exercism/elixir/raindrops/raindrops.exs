defmodule Raindrops do
  @map %{ 3 => "Pling", 5 => "Plang", 7 => "Plong" }

  @doc """
  Returns a string based on raindrop factors.

  - If the number contains 3 as a prime factor, output 'Pling'.
  - If the number contains 5 as a prime factor, output 'Plang'.
  - If the number contains 7 as a prime factor, output 'Plong'.
  - If the number does not contain 3, 5, or 7 as a prime factor,
  just pass the number's digits straight through.
  """
  @spec convert(pos_integer) :: String.t
  def convert(number) do
    ret = factors_for(number)
    |> Enum.uniq
    |> Enum.map(fn p -> Map.get(@map, p, "") end)
    |> Enum.join("")

    if ret == "", do: "#{number}", else: ret
  end

  @spec factors_for(pos_integer) :: [pos_integer]
  def factors_for(1), do: []
  def factors_for(m), do: factor_for(m, 2, []) |> Enum.reverse
  defp factor_for(m, m, acc), do: [m | acc]
  defp factor_for(m, n, acc) when rem(m, n) == 0, do: factor_for(div(m, n), n, [n | acc])
  defp factor_for(m, n, acc), do:  factor_for(m, n+1, acc)
end
