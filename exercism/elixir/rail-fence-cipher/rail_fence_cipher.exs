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
  def encode(str, rails, do_format \\ true) do
    chars = str |> String.split("", trim: true)
    m = List.duplicate([], rails)
    ret = do_encode(chars, 0, rails-1, m, :up)

    if do_format, do: (ret |> format), else: ret
  end

  defp do_encode([], n, limit, acc, _), do: acc
  defp do_encode([h|t], n, limit, acc, dir) do
    r = Enum.at(acc, n)
    m = List.replace_at(acc, n, [h|r])

    case dir do
      :up ->
        if n == limit do
          do_encode(t, n-1, limit, m, :down)
        else
          do_encode(t, n+1, limit, m, :up)
        end
      :down ->
        if n == 0 do
          do_encode(t, n+1, limit, m, :up)
        else
          do_encode(t, n-1, limit, m, :down)
        end
    end
  end

  defp format(m) do
    m |> Enum.map_join("", fn ls ->
      Enum.join(ls |> Enum.reverse, "")
    end)
  end

  @doc """
  Decode a given rail fence ciphertext to the corresponding plaintext
  """
  @spec decode(String.t, pos_integer) :: String.t
  def decode(str, 1), do: str
  def decode(str, rails) do
    chars = str |> String.split("", trim: true)
    m = encode(str, rails, false)
    l = Enum.map(m, &(length(&1)))

    do_decode(l, chars, [])
    |> concat_decoded(0, [], length(chars), :up)
    |> Enum.join("")
  end

  defp do_decode([], _, acc), do: acc |> Enum.reverse
  defp do_decode([h|t], ls, acc) do
    take = Enum.take(ls, h)
    remain = Enum.drop(ls, h)

    do_decode(t, remain, [take|acc])
  end

  defp concat_decoded(m, n, acc, 0, dir), do: acc |> Enum.reverse
  defp concat_decoded(m, n, acc, limit, dir) do
    [h|t] = Enum.at(m, n)
    m = List.replace_at(m, n, t)
    len = length(m) - 1

    case dir do
      :up ->
        if n == len do
          concat_decoded(m, n-1, [h|acc], limit - 1, :down)
        else
          concat_decoded(m, n+1, [h|acc], limit - 1, :up)
        end
      :down ->
        if n == 0 do
          concat_decoded(m, n+1, [h|acc], limit - 1, :up)
        else
          concat_decoded(m, n-1, [h|acc], limit - 1, :down)
        end
    end
  end
end
