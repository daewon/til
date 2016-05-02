defmodule BinarySearch do
  @doc """
  Searches for a key in the list using the binary search algorithm.
  It returns :not_found if the key is not in the list.
  Otherwise returns the tuple {:ok, index}.

  ## Examples

  iex> BinarySearch.search([], 2)
  :not_found

  iex> BinarySearch.search([1, 3, 5], 2)
  :not_found

  iex> BinarySearch.search([1, 3, 5], 5)
  {:ok, 2}

  """

  # It's to impossible to make efficient binary search algorithm in singly linked list
  @spec search(Enumerable.t, integer) :: {:ok, integer} | :not_found

  def search(list, key) do
    ls = Enum.with_index(list) # O(n)
    do_search(ls, key)
  end

  def do_search([], _), do: :not_found
  def do_search([{key, idx}], key), do: {:ok, idx}
  def do_search([{key, idx}], _x), do: :not_found
  def do_search(ls, key) do
    len = length(ls) # O(n)
    mid = div(len, 2)
    {elem, idx} = Enum.at(ls, mid)

    cond do
      elem == key -> {:ok, idx}
      elem > key -> do_search(Enum.take(ls, mid), key)
      elem < key -> do_search(Enum.drop(ls, mid), key)
    end
  end
end
