defmodule Anagram do
  @doc """
  Returns all candidates that are anagrams of, but not equal to, 'base'.
  """
  @spec match(String.t, [String.t]) :: [String.t]
  def match(base, candidates) do
    candidates
    |> Enum.filter(fn target -> anagram?(base, target) end)
  end

  defp hash(word) do
    word
    |> String.downcase
    |> to_char_list
    |> Enum.reduce(%{}, fn ch, acc ->
      Map.merge(acc, %{ch => 1}, fn (_k, v1, v2) -> v1 + v2 end)
    end)
  end

  defp anagram?(base, target) do
    String.downcase(base) !== String.downcase(target)
    and hash(base) === hash(target)
  end
end
