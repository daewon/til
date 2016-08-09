defmodule Flattener do
  @doc """
  Accept a list and return the list flattened without nil values.

  ## Examples

  iex> Flattener.flatten([1, [2], 3, nil])
  [1,2,3]

  iex> Flattener.flatten([nil, nil])
  []

  """

  @spec flatten(list) :: list
  def flatten([]), do: []
  def flatten([[]|t]), do: flatten(t)
  def flatten([nil|t]), do: flatten(t)
  def flatten([h|t]) when is_list(h), do: flatten(h) ++ flatten(t)
  def flatten([h|t]), do: [h|flatten(t)]
end
