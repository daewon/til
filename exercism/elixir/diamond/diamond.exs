defmodule Diamond do
  @doc """
  Given a letter, it prints a diamond starting with 'A',
  with the supplied letter at the widest point.
  """
  @spec build_shape(char) :: String.t
  def build_shape(?A), do: "A\n"
  def build_shape(letter) do
    ls = ?A..letter |> Enum.with_index
    sz = length(ls) - 1

    top = ls
    |> Enum.map(fn {ch, idx} ->
      left_pad = List.duplicate(" ", sz - idx)
      right_pad = List.duplicate(" ", idx)

      left = "#{left_pad}#{<<ch>>}#{right_pad}" |> String.split("", trim: true)
      right = left |> Enum.reverse |> Enum.drop(1) |> Enum.join("") |> String.rstrip

      "#{left}#{right}"
    end)

    bottom = top |> Enum.reverse |> Enum.drop(1)

    diamond = Enum.map_join([top, bottom], "\n", fn ls ->
      Enum.join(ls, "\n")
    end)

    "#{diamond}\n"
  end
end
