defmodule Clock do
  defstruct hour: 0, minute: 0

  @spec py_rem(integer, integer) :: integer
  def py_rem(d, m) do
    r = rem(d, m)

    if r < 0 do
      r + m
    else
      r
    end
  end

  def py_div(d, m) do
    (d / m)
    |> Float.floor
    |> round
  end

  @spec to_minute(Clock) :: integer
  def to_minute(%Clock{hour: h, minute: m}) do
    h * 60 + m
  end

  @spec from_minute(integer) :: Clock
  def from_minute(m) do
    h = py_rem(py_div(m, 60), 24)
    min = py_rem(m, 60)

    %Clock { hour: h, minute: min }
  end

  @doc """
  Returns a string representation of a clock:

  iex> Clock.new(8, 9) |> to_string
  "08:09"
  """
  @spec new(integer, integer) :: Clock
  def new(hour, minute) do
    %Clock { hour: hour, minute: minute }
    |> to_minute
    |> from_minute
  end

  @doc """
  Adds two clock times:

  iex> Clock.add(10, 0) |> Clock.add(3) |> to_string
  "10:03"
  """
  @spec add(Clock, integer) :: Clock
  def add(%Clock{hour: hour, minute: minute}=c, add_minute) do
    min = c |> to_minute
    new_min = add_minute + min
    |> from_minute
  end

  defimpl String.Chars, for: Clock do
    def to_string(%Clock{hour: h, minute: m}) do
      if h < 10 do
        if m < 10 do
          "0#{h}:0#{m}"
        else
          "0#{h}:#{m}"
        end
      else
        if m < 10 do
          "#{h}:0#{m}"
        else
          "#{h}:#{m}"
        end
      end
    end
  end
end
