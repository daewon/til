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

  @doc """
  Calculate a meetup date.

  The schedule is in which week (1..4, last or "teenth") the meetup date should
  fall.
  """
  @spec meetup(pos_integer, pos_integer, weekday, schedule) :: :calendar.date
  def meetup(year, month, weekday, schedule) do
    week_days = find_week_days(year, month, weekday)
    len = length(week_days)
    nth =
      case schedule do
        :first -> 0
        :second -> 1
        :third -> 2
        :fourth -> 3
        :last -> len - 1
        :teenth ->
          cond do
            13 in week_days -> 1
            19 in week_days -> 2
            true -> div(len, 2)
          end
      end

    {year, month, Enum.at(week_days, nth)}
  end

  @schedule [:first, :second, :third, :fourth, :last, :teenth]
  @month_days [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  @days [:monday, :tuesday, :wednesday, :thursday, :friday, :saturday, :sunday]

  def find_week_days(year, month, week_day) do
    days = Enum.at(@month_days, month-1)
    start_day = start_of_month(year, month)
    pad = Enum.find_index(@days, &(&1 == start_day)) - 1

    1..days
    |> Enum.filter(fn n ->
      Enum.at(@days, rem((n + pad), 7)) === week_day
    end)
  end

  defp start_of_year(year) do
    daynum = :calendar.day_of_the_week(year, 1, 1)
    @days |> Enum.at(daynum-1)
  end

  defp start_of_month(year, month) do
    start_of_year = start_of_year(year)
    pad = Enum.find_index(@days, &(&1 === start_of_year))

    remain = @month_days
    |> Enum.take(month-1)
    |> Enum.sum
    |> + pad
    |> rem(7)

    Enum.at(@days, remain)
  end
end
