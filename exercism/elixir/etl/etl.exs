defmodule ETL do
  @doc """
  Transform an index into an inverted index.

  ## Examples

  iex> ETL.transform(%{"a" => ["ABILITY", "AARDVARK"], "b" => ["BALLAST", "BEAUTY"]})
  %{"ability" => "a", "aardvark" => "a", "ballast" => "b", "beauty" =>"b"}
  """
  @spec transform(Map) :: map()
  def transform(input) do
    input
    |> Enum.to_list
    |> Enum.map(fn {k, vs} -> Enum.map(vs, &({String.downcase(&1), k})) end)
    |> List.flatten
    |> Enum.into(%{})
  end
end
