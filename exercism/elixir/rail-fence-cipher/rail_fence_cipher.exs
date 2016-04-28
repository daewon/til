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
  def encode(str, rails) do
    chars = str |> String.split("", trim: true)
    m = List.duplicate([], rails)

    a = do_encode(chars, 0, rails-1, m)

    IO.puts "\n================"
    IO.inspect m
    IO.inspect a
    ""
  end

  defp do_encode([], n, limit, acc), do: acc
  defp do_encode([h|t], n, limit, acc) when n < limit do
    IO.inspect acc
    r = Enum.at(acc, n)
    IO.inspect is_list(r)
    m = List.replace_at(acc, n, [h|r])
    do_encode(t, n+1, limit, [h|acc])
  end
  defp do_encode([h|t], n, limit, acc) when n >= limit do
    r = Enum.at(acc, n)
    m = List.replace_at(acc, n, [h|r])
    do_encode(t, n-1, limit, m)
  end

  @doc """
  Decode a given rail fence ciphertext to the corresponding plaintext
  """
  @spec decode(String.t, pos_integer) :: String.t
  def decode(str, rails) do

  end
end
