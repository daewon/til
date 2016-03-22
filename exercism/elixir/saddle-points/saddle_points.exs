defmodule Matrix do
  @doc """
  Parses a string representation of a matrix
  to a list of rows
  """
  @spec rows(String.t()) :: [[integer]]
  def rows(str) do
    str
    |> String.split("\n", trim: true)
    |> Enum.map(&(String.split(&1, " ", trim: true)))
    |> Enum.map(fn row -> row |> Enum.map(&(String.to_integer(&1))) end)
  end

  @doc """
  Parses a string representation of a matrix
  to a list of columns
  """
  @spec columns(String.t()) :: [[integer]]
  def columns(str) do
    str |> rows |> transpose
  end

  defp transpose([[]|_]), do: []
  defp transpose(matrix) do
    col = matrix |> Enum.map(fn row -> row |> hd end)
    rest = matrix |> Enum.map(fn row -> row |> tl end)
    [col | transpose(rest)]
  end

  @doc """
  Calculates all the saddle points from a string
  representation of a matrix
  """
  @spec saddle_points(String.t()) :: [{integer, integer}]
  def saddle_points(str) do
    {r, c} = {rows(str), columns(str)}
    points = for x <- 0..length(r)-1, y <- 0..length(c)-1, do: {x, y}

    points |> Enum.filter(fn {x, y} ->
      value = r |> Enum.at(x) |> Enum.at(y)
      check_row = r |> Enum.at(x) |> Enum.max === value
      check_column = c |> Enum.at(y) |> Enum.min === value

      check_row and check_column
    end)
  end

  # It's called a "saddle point" because it is greater than or equal to
  # every element in its row and the less than or equal to every element in
  # its column.
end
