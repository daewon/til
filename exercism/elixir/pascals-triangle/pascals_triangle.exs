defmodule PascalsTriangle do
  @doc """
  Calculates the rows of a pascal triangle
  with the given height
  """
  @spec rows(integer) :: [[integer]]
  def rows(num) do
    pascal_stream = Stream.iterate([1], fn prev ->
      next = Enum.chunk(prev, 2, 1)
      |> Enum.map(&Enum.sum/1)

      [1, next, 1] |> List.flatten
    end)

    pascal_stream |> Enum.take(num)
  end
end
