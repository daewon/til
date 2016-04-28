defmodule RailFenceCipher do
  """
  2 -> 1
  A.A.A.....................
  A.A.......................

  3 -> 3
  A...A.....................
  A.A.A.....................
  A...A.....................

  4 -> 5
  T.....I.....O.............
  H...U.C...B...............
  E.Q...K.B.................
  R.....A...................

  5 -> 7
  A.......A.......A.........
  A.....A.A.....A...........
  A...A...A...A.............
  A.A.....A.A...............
  A.......A.................

  6 -> 9
  A.........A...............
  A.......A.A...............
  A.....A...A...............
  A...A.....A...............
  A.A.......A...............
  A.........A...............
  """

  @doc """
  Encode a given plaintext to the corresponding rail fence ciphertext
  """
  @spec encode(String.t, pos_integer) :: String.t
  def encode(str, 1), do: str
  def encode(str, rails) do
    chars = str |> String.split("", trim: true)
    m = List.duplicate([], rails)

    a = do_encode(chars, 0, rails-1, m, :up)

    IO.puts "\n================"
    IO.inspect m
    IO.inspect a

    Enum.map_join(a, "", fn ls ->
      Enum.join(ls |> Enum.reverse, "")
    end)
  end

  defp do_encode([], n, limit, acc, _), do: acc
  defp do_encode([h|t], n, limit, acc, dir) do
    r = Enum.at(acc, n)
    m = List.replace_at(acc, n, [h|r])

    if dir == :up do
      if n == limit do
        do_encode(t, n-1, limit, m, :down)
      else
        do_encode(t, n+1, limit, m, :up)
      end
    else
      if n == 0 do
        do_encode(t, n+1, limit, m, :up)
      else
        do_encode(t, n-1, limit, m, :down)
      end
    end
  end

  @doc """
  Decode a given rail fence ciphertext to the corresponding plaintext
  """
  @spec decode(String.t, pos_integer) :: String.t
  def decode(str, rails) do

  end
end
