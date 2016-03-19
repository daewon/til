defmodule Meetup do
  # # Meetup

  # Calculate the date of meetups.

  # Typically meetups happen on the same day of the week.

  # Examples are

  # - the first Monday
  # - the third Tuesday
  # - the Wednesteenth
  # - the last Thursday

  # Note that "Monteenth", "Tuesteenth", etc are all made up words. There
  # was a meetup whose members realised that there are exactly 7 days that
  # end in '-teenth'. Therefore, one is guaranteed that each day of the week
  # (Monday, Tuesday, ...) will have exactly one date that is named with '-teenth'
  # in every month.
  @moduledoc """
  Calculate meetup dates.
  """

  @type weekday :: :monday | :tuesday | :wednesday | :thursday | :friday | :saturday | :sunday
  @type schedule :: :first | :second | :third | :fourth | :last | :teenth

  @month_days [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  @days [:monday, :tuesday, :wednesday, :thursday, :friday, :saturday, :sunday]

  @doc """
  Calculate a meetup date.

  The schedule is in which week (1..4, last or "teenth") the meetup date should
  fall.
  """
  @spec meetup(pos_integer, pos_integer, weekday, schedule) :: :calendar.date
  def meetup(year, month, weekday, schedule) do
    days = days_of_month(month)
    day_daynum_pairs = days |> Enum.map(fn day ->
      {day, daynum_of(year, month, day)}
    end)
    matchs = day_daynum_pairs |> Enum.filter(fn {_, wd} -> wd === weekday end)

    {day, _} =
      case schedule do
        :first -> matchs |> Enum.at(0)
        :second -> matchs |> Enum.at(1)
        :third -> matchs |> Enum.at(2)
        :fourth -> matchs |> Enum.at(3)
        :last -> matchs |> List.last
        :teenth -> matchs |> Enum.find(&teenth?/1)
      end

    {year, month, day}
  end

  defp days_of_month(month) do
    for day <- 1..Enum.at(@month_days, month-1), do: day
  end

  defp daynum_of(year, month, day) do
    idx = :calendar.day_of_the_week(year, month, day) - 1
    @days |> Enum.at(idx)
  end

  defp teenth?({day, _}), do: day in 13..19
end
