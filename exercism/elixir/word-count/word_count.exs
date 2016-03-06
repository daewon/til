defmodule Words do
  @doc """
  Count the number of words in the sentence.

  Words are compared case-insensitively.
  """
  @spec count(String.t) :: map()
  def count(sentence) do
    to_lower_case = fn(w) -> String.downcase(w) end

    words = Regex.split(~r/[^\w-]|_/iu, sentence, trim: true)
    |> Enum.map(to_lower_case)

    merge_by_word = fn(word, acc) ->
      Map.merge(acc, %{word => 1}, fn _k, a, b -> a + b end)
    end

    Enum.reduce(words, %{}, merge_by_word)
  end
end
