defmodule Queens do
  @type t :: %Queens{ black: {integer, integer}, white: {integer, integer} }
  defstruct black: nil, white: nil
  @board_size 0..7
  @doc """
  Creates a new set of Queens
  """
  @spec new(nil | list) :: Queens.t()
  def new(white: pos, black: pos), do: raise ArgumentError, message: "cannot occupy same space"
  def new(white: {x, y}, black: {cx, cy}), do: %Queens{white: {x, y}, black: {cx, cy}}
  def new, do: %Queens {white: {0, 3}, black: {7, 3}}

  @doc """
  Gives a string reprentation of the board with
  white and black queen locations shown
  """
  @spec to_string(Queens.t()) :: String.t()
  def to_string(queens) do
    tiles = for x <- @board_size, y <- @board_size, do: map_char(queens, {x, y})
    tiles
    |> Enum.chunk(8)
    |> Enum.map(&(Enum.join(&1, " ")))
    |> Enum.join("\n")
  end

  defp map_char(queens, pos) do
    {white, black} = {queens.white, queens.black}
    case pos do
      ^white -> "W"
      ^black -> "B"
      _ -> "_"
    end
  end

  @doc """
  Checks if the queens can attack each other
  """
  @spec can_attack?(Queens.t()) :: boolean
  def can_attack?(q) do
    check_straight(q.white, q.black) or check_diagonal(q.white, q.black)
  end

  defp check_straight({x, y}, {cx, cy}), do: x == cx or y == cy
  defp check_diagonal({x, y}, {cx, cy}), do: abs(x - cx) == abs(y - cy)
end
