defmodule School do
  @moduledoc """
  Simulate students in a school.

  Each student is in a grade.
  """

  @doc """
  Add a student to a particular grade in school.
  """
  @spec add(map, String.t, integer) :: map
  def add(db, name, grade) do
    db |> Map.merge(%{ grade => [name] }, fn _k, curr, acc -> acc ++ curr end)
  end

  @doc """
  Return the names of the students in a particular grade.
  """
  @spec grade(map, integer) :: [String.t]
  def grade(db, grade) do
    db |> Map.get(grade, [])
  end

  @doc """
  Sorts the school by grade and name.
  """
  @spec sort(map) :: [{integer, [String.t]}]
  def sort(db) do
    db
    |> Map.to_list
    |> Enum.sort(fn {a, _}, {c, _} -> a < c end)
    |> Enum.map(fn {k, ls} -> {k, ls |> Enum.sort(fn a, b -> a < b end)} end)
  end
end
