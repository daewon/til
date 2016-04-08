defmodule Minesweeper do
  def make_adjcent_pos({x, y}) do
    [{x-1, y-1}, {x, y-1}, {x+1, y-1},
     {x-1, y},             {x+1, y},
     {x-1, y+1}, {x, y+1}, {x+1, y+1}]
  end

  def parse_board(board) do
    board_with_index = board
    |> Enum.map(fn ls -> String.split(ls, "", trim: true) |> Enum.with_index end)
    |> Enum.with_index

    positions = for {row, x} <- board_with_index, {col, y} <- row do
      mine = if col == "*", do: 1, else: 0
      {{x, y}, mine}
    end

    mine_map = positions |> Enum.into(%{})

    positions = for {{x, y}=pos, mine} <- positions do
      point = make_adjcent_pos(pos) |> Enum.map(fn pos -> mine_map[pos] || 0 end) |> Enum.sum
      {pos, point}
    end

    {board_with_index, positions |> Enum.into(%{})}
  end

  @doc """
  Annotate empty spots next to mines with the number of mines next to them.
  """
  @spec annotate([String.t]) :: [String.t]
  def annotate(board) do
    {board_with_index, mine_map} = parse_board(board)

    board_with_index |> Enum.map(fn {row, x} ->
      Enum.map_join(row, "", fn {col, y} ->
        case {col, mine_map[{x, y}]} do
          {" ", 0} -> " "
          {" ", point} -> point
          _ -> col
        end
      end)
    end)
  end
end
