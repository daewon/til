defmodule Graph do
  defstruct attrs: [], nodes: [], edges: []
end

defmodule Dot do
  defp id(a), do: a
  defp extract_nodes(tree) do
    tree
    |> Enum.flat_map(fn
      {:graph, _meta, _} ->  []
      {atom, _meta, nil} ->  [{atom, []}]
      {atom, _meta, [attr]} -> [{atom, attr}]
      _ -> []
    end)
    |> Enum.sort_by(&id/1)
  end

  defp extract_edges(tree) do
    check_invalid(tree)

    tree
    |> Enum.flat_map(fn
      {:"--", _meta, [{from, _, _}, {to, _, nil} |[]]} -> Macro.escape([{from, to, []}])
      {:"--", _meta, [{from, _, _}, {to, _, [attr]} |[]]} -> Macro.escape([{from, to, attr}])
      _ -> []
    end)
    |> Enum.sort_by(&id/1)
  end

  defp check_invalid(tree) do
    lst = tree |> Enum.map(&Tuple.to_list/1) |> List.flatten
    lst |> Enum.any?(fn
      {:".", _, _} -> raise ArgumentError
      {{:".", _, _}, _, _} -> raise ArgumentError
      _ -> false
    end)
  end

  defp extract_attrs(tree) do
    check_invalid(tree)

    tree
    |> Enum.flat_map(fn
      {:graph, _meta, [attr]} -> attr
      _ -> []
    end)
    |> Enum.sort_by(&id/1)
  end

  defmacro graph(ast) do
    functions = %{:-- => true}
    tree = ast[:do]
    IO.puts("\ntree==================")
    IO.inspect(tree)
    IO.puts("tree==================")

    tree_in_block = cond do
      tree == nil -> []
      elem(tree, 0) == :"__block__" -> tree_in_block = elem(tree, 2)
      true -> tree_in_block = [tree]
    end

    attrs = extract_attrs(tree_in_block)
    edges = extract_edges(tree_in_block)
    nodes = extract_nodes(tree_in_block)

    quote do
      %Graph{attrs: unquote(attrs), nodes: unquote(nodes), edges: unquote(edges)}
    end
  end
end
