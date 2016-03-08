defmodule ListOps do
  # Please don't use any external modules (especially List) in your
  # implementation. The point of this exercise is to create these basic functions
  # yourself.
  #
  # Note that `++` is a function from an external module (Kernel, which is
  # automatically imported) and so shouldn't be used either.

  @spec count(list) :: non_neg_integer
  def count([]), do: 0
  def count([h | t]), do: 1 + count(t)

  @spec reverse(list) :: list
  def reverse([]), do: []
  def reverse(ls), do: reverse(ls, [])
  def reverse([h | t], acc), do: reverse(t, [h | acc])
  def reverse([], acc), do: acc

  @spec map(list, (any -> any)) :: list
  def map([], _), do: []
  def map([h | t], f), do: [f.(h) | map(t, f)]

  @spec filter(list, (any -> as_boolean(term))) :: list
  def filter([], _), do: []
  def filter([h | t], f) do
    case f.(h) do
      true -> [h | filter(t, f)]
      _ -> filter(t, f)
    end
  end

  @type acc :: any
  @spec reduce(list, acc, ((any, acc) -> acc)) :: acc
  def reduce([], acc, _), do: acc
  def reduce([h | t], acc, f), do: reduce(t, f.(h, acc), f)

  @spec append(list, list) :: list
  def append([], []), do: []
  def append(a, []), do: a
  def append([], b), do: b
  def append([h | t], b), do: [h | append(t, b)]

  @spec concat([[any]]) :: [any]
  def concat([]), do: []
  def concat([h | t]) when is_map(h), do: concat([List.to_list(h) | t])
  def concat([h | t]) when is_list(h), do: append(h, concat(t))
  def concat([h | t]), do: [h | concat(t)]
end
