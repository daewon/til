defmodule Atbash do
  @alpha  "abcdefghijklmnopqrstuvwxyz1234567890" |> String.split("")
  @cipher "zyxwvutsrqponmlkjihgfedcba1234567890" |> String.split("")

  @encode_map Enum.zip(@alpha, @cipher) |> Enum.into(%{})
  @doc """
  Encode a given plaintext to the corresponding ciphertext

  ## Examples

  iex> Atbash.encode("completely insecure")
  "xlnko vgvob rmhvx fiv"
  """
  @spec encode(String.t) :: String.t
  def encode(plaintext) do
    plaintext
    |> String.downcase
    |> String.split("", trim: true)
    |> Enum.filter(&( @encode_map[&1] ))
    |> Enum.map(&( @encode_map[&1] ))
    |> Enum.chunk(5, 5, [])
    |> Enum.map_join(" ", &(Enum.join(&1, "")))
  end
end
