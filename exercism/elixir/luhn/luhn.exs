defmodule Luhn do
  @doc """
  Calculates the total checksum of a number
  """
  @spec checksum(String.t()) :: integer
  def checksum(number) do
    digits = number
    |> String.to_integer
    |> Integer.digits
    |> Enum.reverse
    |> Enum.with_index
    |> Enum.reduce(0, fn {n, i}, acc ->
      if rem(i, 2) == 0 do
        n + acc
      else
        double = n * 2
        num = case double > 10 do
                true -> double - 9
                _ -> double
              end
        num + acc
      end
    end)

    # odd_sum = odd_pos |> Enum.sum
    # even_sum = even_pos |> Enum.map(fn n ->
    #   double = n * 2
      #   case double > 10 do
      #     true -> double - 9
    #     _ -> double
    #   end

    # end) |> Enum.sum
    # even_sum + odd_sum
  end

  @doc """
  Checks if the given number is valid via the luhn formula
  """
  @spec valid?(String.t()) :: boolean
  def valid?(number) do
    last_num = number
    |> checksum
    |> Integer.digits
    |> List.last

    last_num == 0
  end

  @doc """
  Creates a valid number by adding the correct
  checksum digit to the end of the number
  """
  @spec create(String.t()) :: String.t()
  def create(number) do
    last_num = number
    |> checksum
    |> Integer.digits
    |> List.last
    IO.puts("\n=========")
    IO.inspect checksum(number)

    "#{number}#{last_num}"
  end
end
