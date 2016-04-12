defmodule CustomSet do
  defstruct list: []

  # This lets the compiler check that all Set callback functions have been
  # implemented.
  @behaviour Set

  def delete(%CustomSet{list: list}, item) do
    %CustomSet{list: list -- [item]}
  end

  def difference(a, b) do
    a.list
    |> Enum.reject(fn item -> item in b.list end)
    |> new
  end

  def disjoint?(a, b) do
    equal?(difference(a, b), a)
  end

  def equal?(a, b) do
    Enum.sort(a.list) == Enum.sort(b.list)
  end

  def intersection(a, b) do
    a.list
    |> Enum.filter(fn item -> item in b.list end)
    |> new
  end

  def member?(set, item) do
    item in set.list
  end

  def new, do: %CustomSet{}
  def new(ls), do: %CustomSet{list: ls |> Enum.uniq |> Enum.sort}

  def put(set, item) do
    [item | set.list] |> new
  end

  def size(set) do
    set.list |> length
  end

  def subset?(a, b) do
    a.list |> Enum.all?(fn item -> item in b.list end )
  end

  def to_list(set) do
    set.list
  end

  def union(a, b), do: new(a.list ++ b.list)

  def empty, do: %CustomSet{}
  def empty(_ls), do: %CustomSet{}
end
