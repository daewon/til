defmodule Year do
  @doc """
  Returns whether 'year' is a leap year.

  A leap year occurs:

  on every year that is evenly divisible by 4
  except every year that is evenly divisible by 100
  except every year that is evenly divisible by 400.
  """
  @spec leap_year?(non_neg_integer) :: boolean
  def leap_year?(year) do
    is = fn(y) -> rem(year, y) === 0 end
    is_not = fn(a) -> not is.(a) end

    cond do
      is.(4) and is.(100) and is_not.(400)-> false
      is.(4) -> true
      is.(400) -> true
      true -> false
    end
  end
end
