defmodule Gigasecond do
  @giga_seconds 1_000_000_000

  @doc """
  Calculate a date one billion seconds after an input date.
  """
  @spec from({{pos_integer, pos_integer, pos_integer}, {pos_integer, pos_integer, pos_integer}}) :: :calendar.datetime
  def from({{year, month, day}, {hours, minutes, seconds} = hour}) do
    :calendar.date_to_gregorian_days(year, month, day)
    |> days_to_seconds
    |> + times_to_seconds(hour)
    |> + @giga_seconds
    |> :calendar.gregorian_seconds_to_datetime
  end

  defp times_to_seconds({h, m, s}), do: (h * 60 * 60) + (m * 60) + s
  defp days_to_seconds(days), do: days * 24 * 60 * 60
end
