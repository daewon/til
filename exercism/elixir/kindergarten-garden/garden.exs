defmodule Garden do
  defstruct alice: [], bob: [], charlie: [], david: [], eve: [], fred: [], ginny: [], harriet: [], ileana: [], joseph: [], kincaid: [], larr: [], nate: [], maggie: [], ophelia: []

  @doc """
  Accepts a string representing the arrangement of cups on a windowsill and a
  list with names of students in the class. The student names list does not
  have to be in alphabetical order.

  It decodes that string into the various gardens for each student and returns
  that information in a map.
  """
  @garden_map %{"R" => :radishes, "C" => :clover, "G" => :grass, "V" => :violets}

  @childrens ~w{alice bob charlie david eve fred ginny harriet ileana joseph kincaid larry}a

  @spec info(String.t(), list) :: map
  def info(info_string), do: info(info_string, @childrens)
  def info(info_string, student_names) do
    m = info_string
    |> String.split("\n", trim: true)
    |> Enum.map(fn l -> String.split(l, "", trim: true) end)
    # [[R, C], [G, G]]

    childrens_with_idx = student_names |> Enum.sort |> Enum.with_index

    maps = childrens_with_idx
    |> Enum.map(fn {nm, idx} ->
      [r1, r2] = m

      sz = idx * 2
      a = Enum.drop(r1, sz) |> Enum.take(2)
      b = Enum.drop(r2, sz) |> Enum.take(2)

      ls = [a, b]
      |> List.flatten
      |> Enum.map(fn k -> @garden_map[k] end)
      |> List.to_tuple

      {nm, ls}
    end)

    maps
    |> Enum.reduce(%Garden{}, fn {nm, ls}, acc ->
      Map.merge(acc, %{nm => ls})
    end)

  end
end
