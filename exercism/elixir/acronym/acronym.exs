defmodule Acronym do
  @doc """
  Generate an acronym from a string.
  "This is a string" => "TIAS"
  """
  @spec abbreviate(string) :: String.t()
  def abbreviate(string) do
    Regex.scan(~r/([A-Z])[a-z]+?|\s([a-zA-Z])+?/, string, trim: true)
    |> Enum.map(fn(ls) -> List.last(ls) end)
    |> Enum.join("")
    |> String.upcase
  end
end
