defmodule Scrabble do
  @score [
    {1, [?A, ?E, ?I, ?O, ?U, ?L, ?N, ?R, ?S, ?T]},
    {2, [?D, ?G]},
    {3, [?B, ?C, ?M, ?P]},
    {4, [?F, ?H, ?V, ?W, ?Y]},
    {5, [?K]},
    {8, [?J, ?X]},
    {10,[?Q, ?Z]}
  ]

  @score_map @score |>
    Enum.flat_map(fn({k, l}) -> Enum.map(l, &({&1, k})) end) |> Enum.into(%{})

  @doc """
  Calculate the scrabble score for the word.
  """
  @spec score(String.t) :: non_neg_integer
  def score(word) do
    word
    |> String.upcase
    |> to_char_list
    |> Enum.map(fn ch -> @score_map[ch] || 0 end)
    |> Enum.sum
  end
end
