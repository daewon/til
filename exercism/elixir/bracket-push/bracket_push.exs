defmodule BracketPush do
  @open ["{", "[", "("]
  @close ["}", "]", ")"]

  @open_pair Enum.zip(@open, @close) |> Enum.into(%{})
  @close_pair Enum.zip(@close, @open) |> Enum.into(%{})

  @doc """
  Checks that all the brackets and braces in the string are matched correctly, and nested correctly
  """
  @spec check_brackets(String.t) :: boolean
  def check_brackets(str) do
    chars = str
    |> String.split("", trim: true)
    |> Enum.filter(fn ch ->
      Map.has_key?(@open_pair, ch) or Map.has_key?(@close_pair, ch)
    end)

    chars |> check_match([])
  end

  defp check_match([], []), do: true
  defp check_match([], ls), do: false
  defp check_match([h|t], []) do
    open = Map.has_key?(@open_pair, h)
    if open, do: check_match(t, [h]), else: false
  end
  defp check_match([h|t], [sh|st]=stack) do
    case {@open_pair[h], @close_pair[h], sh} do
      {_, a, a} -> check_match(t, st)
      {a, _, _} -> check_match(t, [h|stack])
    end
  end
end
