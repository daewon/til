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
    |> Map.to_list
    |> Enum.reduce(%{}, fn {k, ls}, acc ->
      ls
      |> Enum.map(&String.downcase/1)
      |> Enum.reduce(acc, fn word, acc ->
        Map.merge(%{word => k}, acc)
      end)
    end)
  end
end
