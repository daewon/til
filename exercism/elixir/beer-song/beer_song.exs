defmodule BeerSong do
  def bottles(0), do: "bottle"
  def bottles(_), do: "bottles"

  @doc """
  Get a single verse of the beer song
  """
  @spec verse(integer) :: String.t
  def verse(1) do
  """
  No more bottles of beer on the wall, no more bottles of beer.
  Go to the store and buy some more, 99 bottles of beer on the wall.
  """
  end

  def verse(n) do
  """
  #{n - 1} #{bottles(n - 2)} of beer on the wall, #{n - 1} #{bottles(n - 2)} of beer.
  #{bootle_of_wall(n - 2)}
  """
  end

  def bootle_of_wall(0) do
    "Take it down and pass it around, no more bottles of beer on the wall."
  end
  def bootle_of_wall(n) do
    "Take one down and pass it around, #{n} #{bottles(n - 1)} of beer on the wall."
  end

  @doc """
  Get the entire beer song for a given range of numbers of bottles.
  """
  @spec lyrics(Range.t) :: String.t
  def lyrics(range \\ 100..1) do
    range
    |> Enum.map(&verse/1)
    |> Enum.join("\n")
  end
end
