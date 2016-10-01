defmodule Markdown do
  @doc """
  Parses a given string with Markdown syntax and returns the associated HTML for that string.

  ## Examples

  iex> Markdown.parse("This is a paragraph")
  "<p>This is a paragraph</p>"

  iex> Markdown.parse("#Header!\n* __Bold Item__\n* _Italic Item_")
  "<h1>Header!</h1><ul><li><em>Bold Item</em></li><li><i>Italic Item</i></li></ul>"
  """

  def process_em_i(line) do
    new_line = Regex.replace(~r/__(.*?)__/, line, "<em>\\1</em>")
    Regex.replace(~r/_(.*?)_/, new_line, "<i>\\1</i>")
  end

  defp span(ls) do
    count = Enum.take(ls, 1) |> hd |> String.length
    line = Enum.drop(ls, 1)

    [count, line]
  end

  def process_head(ln) do
    [count, chars] = span(String.split(ln, " "))
    str = Enum.join(chars, " ")

    "<h#{count}>#{str}</h#{count}>"
  end

  def process_list(ln) do
    [_, chars] = span(String.split(ln, " "))
    str = Enum.join(chars, " ")

    "<li>#{str}</li>"
  end

  def process_paragraph(ln), do: "<p>#{ln}</p>"

  def process_group_list(lss) do
    cluster = Enum.chunk_by(lss, fn ls -> String.starts_with?(ls, "<li>") end)

    cluster
    |> Enum.flat_map(fn ls ->
      case length ls do
        1 -> ls
        _ -> ["<ul>" | ls] ++ ["</ul>"]
      end
    end)
  end

  @spec parse(String.t) :: String.t
  def parse(markdown) do
    lines = markdown |> String.split("\n")

    lines
    |> Enum.map(fn ln ->
      chars = String.split(ln, "", trim: true)
      with_em_i = ln |> process_em_i

      case hd chars do
        "#" -> with_em_i |> process_head
        "*" -> with_em_i |> process_list
        _ ->   with_em_i |> process_paragraph
      end
    end)
    |> process_group_list
    |> Enum.join("")

  end
end
