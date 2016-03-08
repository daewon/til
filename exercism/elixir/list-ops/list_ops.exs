defmodule ListOps do
  # Please don't use any external modules (especially List) in your
  # implementation. The point of this exercise is to create these basic functions
  # yourself.
  #
  # Note that `++` is a function from an external module (Kernel, which is
  # automatically imported) and so shouldn't be used either.

  @spec count(list) :: non_neg_integer
  def count(l) do
    trav = fn(ls, trav) ->
      case ls do
        [h | t] -> 1 + trav.(t, trav)
        [] -> 0
      end
    end

    trav.(l, trav)
  end

  @spec reverse(list) :: list
  def reverse(l) do
    trav = fn(ls, acc, trav) ->
      case ls do
        [h | t] -> trav.(t, [h | acc], trav)
        [] -> acc
      end
    end

    trav.(l, [], trav)
  end

  @spec map(list, (any -> any)) :: list
  def map(l, f) do
    trav = fn(ls, trav) ->
      case ls do
        [h | t] -> [f.(h) | trav.(t,trav)]
        [] -> []
      end
    end

    trav.(l, trav)
  end

  @spec filter(list, (any -> as_boolean(term))) :: list
  def filter(l, f) do
    trav = fn(ls, trav) ->
      case ls do
        [h | t] ->
          case f.(h)  do
            true -> [h | trav.(t,trav)]
            _ -> trav.(t,trav)
          end
        [] -> []
      end
    end

    trav.(l, trav)
  end

  @type acc :: any
  @spec reduce(list, acc, ((any, acc) -> acc)) :: acc
  def reduce(l, acc, f) do
    trav = fn(ls, acc, trav) ->
      case ls do
        [h | t] -> trav.(t, f.(h, acc), trav)
        [] -> acc
      end
    end

    trav.(l, acc, trav)
  end

  @spec append(list, list) :: list
  def append(a, b) do
    trav = fn(ls, trav) ->
      case ls do
        [h | t] -> [h | trav.(t, trav)]
        [] -> b
      end
    end

    trav.(a, trav)
  end

  @spec concat([[any]]) :: [any]
  def concat(ll) do
    trav = fn(ls, trav) ->
      lss = cond do
        is_map(ls) -> List.to_list(ls)
        true -> ls
      end

      case lss do
        [h | t] ->
          case is_list(h) do
            true -> append(h, trav.(t, trav))
            false -> [h | trav.(t, trav)]
          end
        [] -> []
      end
    end

    trav.(ll, trav)
  end
end
