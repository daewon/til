defmodule Triangle do
  @type kind :: :equilateral | :isosceles | :scalene

  @doc """
  Return the kind of triangle of a triangle with 'a', 'b' and 'c' as lengths.
  """
  @spec kind(number, number, number) :: { :ok, kind } | { :error, String.t }
  def kind(ax, bx, cx), do: Kernel.apply(&kind2/3, Enum.sort([ax, bx, cx]))

  defp kind2(a, _, _) when a <= 0, do: { :error, "all side lengths must be positive" }
  defp kind2(a, b, c) when a + b <= c, do: { :error, "side lengths violate triangle inequality" }
  defp kind2(a, a, a), do: { :ok, :equilateral }
  defp kind2(_, b, b), do: { :ok, :isosceles }
  defp kind2(a, b, c), do: { :ok, :scalene }
end
