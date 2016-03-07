defmodule RunLengthEncoder do
  @doc """
  Generates a string where consecutive elements are represented as a data value and count.
  "HORSE" => "1H1O1R1S1E"
  For this example, assume all input are strings, that are all uppercase letters.
  It should also be able to reconstruct the data into its original form.
  "1H1O1R1S1E" => "HORSE"
  """
  @spec encode(string) :: String.t
  def encode(string) do
    ls = string |> String.split("", trim: true)

    enc = fn (ls, enc) ->
      case ls do
        [] -> []
        [h|_] ->
          group = Enum.take_while(ls, fn(a) -> a == h end)
        tl = enc.(Enum.drop_while(ls, fn(a) -> a == h end), enc)
        [group | tl]
      end
    end

    enc.(ls, enc)
    |> Enum.reject(fn(ls) -> length(ls) == 0 end)
    |> Enum.map(fn(ls) -> "#{length(ls)}#{hd(ls)}" end)
    |> Enum.join("")
  end

  @spec decode(string) :: String.t
  def decode(string) do
    ls = Regex.scan(~r/([0-9]+)([\D])/, string)
    |> Enum.map(fn(ls) -> tl(ls) end)
    |> Enum.map(fn([h|t]) ->
      count = String.to_integer(h)
      char = List.first(t)
      String.duplicate(char, count)
    end)
    |> Enum.join("")
  end
end
