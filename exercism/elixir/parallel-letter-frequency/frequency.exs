defmodule Frequency do
  @doc """
  Count letter frequency in parallel.

  Returns a dict of characters to frequencies.

  The number of worker processes to use can be set with 'workers'.
  """
  @spec frequency([String.t], pos_integer) :: map
  def frequency(texts, workers) do
    texts
    |> Enum.flat_map(fn str -> String.split(str, "", trim: true) end)
    |> Enum.filter(&( &1 != " " and &1 != "," and ~r/\d/ != &1 ) )
    |> Enum.map(&String.downcase/1)
    |> Enum.group_by(fn a -> a end)
    |> Enum.map(fn {k, ls} -> {k, length(ls)} end)
  end
end
