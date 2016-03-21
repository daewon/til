defmodule Queens do
  @type t :: %Queens{ black: {integer, integer}, white: {integer, integer} }
  defstruct black: nil, white: nil

  @doc """
  Creates a new set of Queens
  """
  @spec new(nil | list) :: Queens.t()
  def new(positions \\ nil) do
    case positions do
      [white: a, black: a] -> raise ArgumentError, message: "cannot occupy same space"
      [white: {a, b}, black: {c, d}] -> %Queens { white: {a, b}, black: {c, d} }
      nil -> %Queens { white: {0, 3}, black: {7, 3} }
    end
  end

  @doc """
  Gives a string reprentation of the board with
  white and black queen locations shown
  """
  @spec to_string(Queens.t()) :: String.t()
  def to_string(queens) do
    {blank, white, black, rng} = {"_", "W", "B", 0..7}
    %Queens{black: black_point, white: white_point} = queens
    points = for x <- rng, y <- rng, do: {x, y}

    points
    |> Enum.map(fn {x, y}=point ->
      case point do
        ^white_point -> white
        ^black_point -> black
        _ -> blank
      end
    end)
    |> Enum.chunk(8)
    |> Enum.map(&(Enum.join(&1, " ")))
    |> Enum.join("\n")
  end

  @doc """
  Checks if the queens can attack each other
  """
  @spec can_attack?(Queens.t()) :: boolean
  def can_attack?(queens) do
    %Queens{ black: {bx, by}, white: {wx, wy} } = queens

    cond do
      bx === wx -> true
      by === wy -> true
      true -> check_cross({bx, by}, {wx, wy})
    end
  end

  defp check_cross({x, y}, {tx, ty}) do
    check_cross({x, y}, {tx, ty}, :ll)
    or check_cross({x, y}, {tx, ty}, :lr)
    or check_cross({x, y}, {tx, ty}, :rr)
    or check_cross({x, y}, {tx, ty}, :rl)
  end

  defp check_cross({8, _}, _, _), do: false
  defp check_cross({_, 8}, _, _), do: false
  defp check_cross({-1, _}, _, _), do: false
  defp check_cross({_, -1}, _, _), do: false
  defp check_cross({x, y}, {x, y}, dir), do: true
  defp check_cross({x, y}, {tx, ty}, dir) do
    case dir do
      :ll -> check_cross({x+1, y+1}, {tx, ty}, :ll)
      :lr -> check_cross({x-1, y-1}, {tx, ty}, :lr)
      :rr -> check_cross({x-1, y+1}, {tx, ty}, :rr)
      :rl -> check_cross({x+1, y-1}, {tx, ty}, :rl)
    end
  end
end
