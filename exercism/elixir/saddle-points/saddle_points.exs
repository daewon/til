defmodule Matrix do
  defp flip_tuple({a, b}), do: {b, a}

  @doc """
  Parses a string representation of a matrix
  to a list of rows
  """
  @spec rows(String.t()) :: [[integer]]
  def rows(str) do
    str
    |> String.split("\n", trim: true)
    |> Enum.map(&(String.split(&1, " ", trim: true)))
    |> Enum.map(fn row -> row |> Enum.map(&String.to_integer/1) end)
  end

  def rows(str, with_max: true) do
    rs = rows(str)
    max_rs = rs
    |> Enum.map(&Enum.max/1)
    |> Enum.with_index
    |> Enum.map(&flip_tuple/1)
    |> Enum.into(%{})

    {rs, max_rs}
  end

  @doc """
  Parses a string representation of a matrix
  to a list of columns
  """
  @spec columns(String.t()) :: [[integer]]
  def columns(str), do: str |> rows |> transpose
  def columns(str, with_min: true) do
    cl = columns(str)
    min_cl = cl
    |> Enum.map(&Enum.min/1)
    |> Enum.with_index
    |> Enum.map(&flip_tuple/1)
    |> Enum.into(%{})

    {cl, min_cl}
  end

  defp transpose([[]|_]), do: []
  defp transpose(matrix) do
    col = matrix |> Enum.map(&hd/1)
    rest = matrix |> Enum.map(&tl/1)

    [col | transpose(rest)]
  end

  @doc """
  Calculates all the saddle points from a string
  representation of a matrix

  It's called a "saddle point" because it is greater than or equal to
  every element in its row and the less than or equal to every element in
  its column.
  """
  @spec saddle_points(String.t()) :: [{integer, integer}]
  def saddle_points(str) do
    {rs, max_row_map} = rows(str, with_max: true)
    {_cs, min_col_map} = columns(str, with_min: true)
    map = for {r, i} <- Enum.with_index(rs), {c, j} <- Enum.with_index(r), do: {{i, j}, c}

    Enum.into(map, %{})
    |> Enum.filter(fn {{r, c}, v} ->
      valid?(max_row_map, r, v) and valid?(min_col_map, c, v)
    end)
    |> Enum.map(fn({point, _}) -> point end)
  end

  defp valid?(map, k, v), do: map[k] === v
end
