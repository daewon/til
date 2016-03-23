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
  def rows(str, with_max: true) do
    rs = rows(str)
    max_rs = rs |> Enum.map(&Enum.max/1)
    {rs, max_rs}
  end

  @doc """
  Parses a string representation of a matrix
  to a list of columns
  """
  @spec columns(String.t()) :: [[integer]]
  def columns(str) do
    str |> rows |> transpose
  end
  def columns(str, with_min: true) do
    cl = columns(str)
    min_cl = cl |> Enum.map(&Enum.min/1)
    {cl, min_cl}
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
    {rs, maxes} = rows(str, with_max: true)
    {cs, mins} = columns(str, with_min: true)
    map = for {r, i} <- Enum.with_index(rs), {c, j} <- Enum.with_index(r), do: {{i, j}, c}

    Enum.into(map, %{})
    |> Enum.filter(fn {{r, c}, v} ->
      is_max = Enum.at(maxes, r) === v
      is_min = Enum.at(mins, c) === v
      is_max and is_min
    end)
    |> Enum.map(fn({point, _}) -> point end)
  end

  # It's called a "saddle point" because it is greater than or equal to
  # every element in its row and the less than or equal to every element in
  # its column.
end
