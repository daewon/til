defmodule Connect do
  @doc """
  Calculates the winner (if any) of a board
  using "O" as the white player
  and "X" as the black player
  """
  @spec result_for([String.t]) :: :none | :black | :white
  def result_for(board) do
    map = parse_board(board)
    {height, len} = {length(board)-1, (board |> hd |> String.length)-1}
    {o_starts, x_starts} = {get_start_points(map, "O"), get_start_points(map, "X")}

    fn_traverse_start = fn goal -> fn {pos, go} ->
        traverse(map, pos, go, %{pos => true}, goal)
      end
    end

    o_connected = Enum.flat_map(o_starts, fn_traverse_start.(height))
    x_connected = Enum.flat_map(x_starts, fn_traverse_start.(len))

    case {length(o_connected) > 0, length(x_connected) > 0} do
      {true, _} -> :white
      {_, true} -> :black
      {_, _} -> :none
    end
  end

  defp do_traverse(map, pos, go, visited, goal) do
    positions = pos
    |> make_adjacent
    |> Enum.reject(fn pos -> visited[pos] end)
    |> Enum.filter(fn pos -> map[pos] == go end)

    positions
    |> Enum.flat_map(fn pos -> traverse(map, pos, go, Map.put(visited, pos, true), goal) end)
  end

  def traverse(map, pos, "X"=go, visited, goal) do
    case {pos, map[pos]} do
      {{_, ^goal}, ^go}-> [:black]
      _ -> do_traverse(map, pos, go, visited, goal)
    end
  end

  def traverse(map, pos, "O"=go, visited, goal) do
    case {pos, map[pos]} do
      {{^goal, _}, ^go}-> [:white]
      _ -> do_traverse(map, pos, go, visited, goal)
    end
  end

  def parse_board(board) do
    board_with_index = board
    |> Enum.map(&String.graphemes/1)
    |> Enum.map(&Enum.with_index/1)
    |> Enum.with_index

    positions = for {row, r} <- board_with_index, {col, c} <- row do
      {{r, c}, col}
    end

    map = positions |> Enum.into(%{})
  end

  def make_adjacent({x, y}) do
    [{x-1, y-1}, {x, y-1}, {x+1, y-1},
     {x-1, y},             {x+1, y},
     {x-1, y+1}, {x, y+1}, {x+1, y+1}]
  end

  def get_start_points(map, "O")  do
    map |> Enum.filter(fn
      {{0, _}, "O"} -> true
      _ -> false
    end)
  end

  def get_start_points(map, "X")  do
    map |> Enum.filter(fn
      {{_, 0}, "X"} -> true
      _ -> false
    end)
  end
end
