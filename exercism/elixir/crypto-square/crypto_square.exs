defmodule CryptoSquare do
  @doc """
  Encode string square methods
  ## Examples

  iex> CryptoSquare.encode("abcd")
  "ac bd"
  """
  @spec encode(String.t) :: String.t
  def encode(str) do
    str
    |> normalize
    |> to_matrix
    |> transpose
    |> format
  end

  defp format(m) do
    m
    |> Enum.map(&(Enum.join(&1, "")))
    |> Enum.join(" ")
  end

  defp transpose([], _), do: []
  defp transpose([[]|_], acc),  do: Enum.reverse(acc)
  defp transpose(m, acc \\ []) do
    cols = Enum.map(m, &hd/1)
    remains = Enum.map(m, &tl/1)

    transpose(remains, [cols|acc])
  end

  defp to_matrix([]), do: []
  defp to_matrix(ls) do
    chunk_size = get_column_size(ls)
    pad = List.duplicate("", chunk_size)

    Enum.chunk(ls, chunk_size, chunk_size, pad)
  end

  defp normalize(str) do
    str
    |> String.downcase
    |> String.split("", trim: true)
    |> Enum.filter(&(Regex.match?(~r/[a-z\d]/, &1)))
  end

  defp get_column_size(ls) do
    ls
    |> length
    |> :math.sqrt
    |> Float.ceil
    |> Kernel.round
  end
end
