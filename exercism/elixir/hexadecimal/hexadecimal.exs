defmodule Hexadecimal do
  @doc """
  Accept a string representing a hexadecimal value and returns the
  corresponding decimal value.
  It returns the integer 0 if the hexadecimal is invalid.
  Otherwise returns an integer representing the decimal value.

  ## Examples

  iex> Hexadecimal.to_decimal("invalid")
  0

  iex> Hexadecimal.to_decimal("af")
  175

  """

  @hex ~w(0 1 2 3 4 5 6 7 8 9 A B C D E F)
  @hex_map Enum.with_index(@hex) |> Enum.into(%{})

  @spec to_decimal(binary) :: integer
  def to_decimal(hex) do
    chars = hex |> String.upcase |> String.split("", trim: true)

    if valid?(chars) do
      chars
      |> Enum.reverse
      |> Enum.with_index
      |> Enum.reverse
      |> Enum.map(fn {v, i} ->
        @hex_map[v] * :math.pow(16, i)
      end)
      |> Enum.sum
    else
      0
    end
  end

  defp valid?(chars) do
    chars |> Enum.all?(fn ch -> @hex_map[ch] != nil end)
  end
end
