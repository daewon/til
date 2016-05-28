defmodule Luhn do
  @doc """
  Calculates the total checksum of a number
  """
  @spec checksum(String.t()) :: integer
  def checksum(number) do
    reverse_with_indexed = number
    |> String.to_integer
    |> Integer.digits
    |> Enum.reverse
    |> Enum.with_index

    reverse_with_indexed |> Enum.reduce(0, fn {n, i}, acc ->
      if rem(i, 2) == 0 do
        n + acc # for even position
      else
        double = n * 2
        num = if double > 10, do: double - 9, else: double
        num + acc
      end
    end)
  end

  @doc """
  Checks if the given number is valid via the luhn formula
  """
  @spec valid?(String.t()) :: boolean
  def valid?(number) do
    last_num = number |> checksum |> rem(10)
    last_num == 0
  end

  @doc """
  Creates a valid number by adding the correct
  checksum digit to the end of the number
  """
  @spec create(String.t()) :: String.t()
  def create(number) do
    ls_valid = 1..9
    |> Enum.map(fn n -> "#{number}#{n}" end)
    |> Enum.filter(&valid?/1)

    if Enum.empty?(ls_valid), do: "#{number}0", else: List.first(ls_valid)
  end
end
