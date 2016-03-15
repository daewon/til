defmodule Phone do
  @doc """
  Remove formatting from a phone number.

  Returns "0000000000" if phone number is not valid
  (10 digits or "1" followed by 10 digits)

  ## Examples

  iex> Phone.number("123-456-7890")
  "1234567890"

  iex> Phone.number("+1 (303) 555-1212")
  "3035551212"

  iex> Phone.number("867.5309")
  "0000000000"
  """
  @spec number(String.t) :: String.t
  def number(raw) do
    [h|t] = ~r/\d/ |> Regex.scan(raw, trim: true) |> List.flatten

    cond do
      raw =~ ~r/[a-z]/ -> "0000000000"
      length([h|t]) === 10 -> Enum.join([h|t], "")
      h === "1" and length(t) > 9 -> Enum.join(t, "")
      true -> "0000000000"
    end
  end

  @doc """
  Extract the area code from a phone number

  Returns the first three digits from a phone number,
  ignoring long distance indicator

  ## Examples

  iex> Phone.area_code("123-456-7890")
  "123"

  iex> Phone.area_code("+1 (303) 555-1212")
  "303"

  iex> Phone.area_code("867.5309")
  "000"
  """
  @spec area_code(String.t) :: String.t
  def area_code(raw) do
    raw |> number |> to_char_list |> Enum.take(3) |> to_string
  end

  @doc """
  Pretty print a phone number

  Wraps the area code in parentheses and separates
  exchange and subscriber number with a dash.

  ## Examples

  iex> Phone.pretty("123-456-7890")
  "(123) 456-7890"

  iex> Phone.pretty("+1 (303) 555-1212")
  "(303) 555-1212"

  iex> Phone.pretty("867.5309")
  "(000) 000-0000"
  """
  @spec pretty(String.t) :: String.t
  def pretty(raw) do
    normalized = raw |> number |> to_char_list
    [a | [b | [c | []]]] = group(normalized, 3, 2) |> Enum.map(&to_string/1)
    "(#{a}) #{b}-#{c}"
  end

  defp group(ls, _, 0), do: [ls]
  defp group(ls, sz, n), do: [Enum.take(ls, sz) | group(Enum.drop(ls, sz), sz, n-1)]
end
