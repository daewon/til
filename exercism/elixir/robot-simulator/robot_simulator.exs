defmodule RobotSimulator do

  defstruct direction: :north, position: {0, 0}

  @valid_directions [:north, :east, :south, :west]

  @doc """
  Create a Robot Simulator given an initial direction and position.
  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  @spec create(direction :: atom, position :: { integer, integer }) :: any
  def create(direction \\ :north, position \\ {0, 0}) do
    case position do
      {x, y} when is_integer(x) and is_integer(y) ->
        if direction in @valid_directions do
          %RobotSimulator{direction: direction,position: position}
        else
          {:error, "invalid direction"}
        end
      _ -> {:error, "invalid position"}
    end
  end

  @doc """
  Simulate the robot's movement given a string of instructions.

  Valid instructions are: "R" (turn right), "L", (turn left), and "A" (advance)
  """
  @spec simulate(robot :: any, instructions :: String.t ) :: any
  def simulate(robot, instructions) do
    insts = instructions
    |> String.split("", trim: true)
    is_valid = insts |> Enum.all?(fn a -> a == "L" or a == "R" or a == "A" end)

    if is_valid do
      insts
      |> Enum.reduce(robot, fn
        "L", acc -> %{acc | direction: rl(acc.direction)}
        "R", acc -> %{acc | direction: rr(acc.direction)}
        "A", acc ->
          {x, y} = acc.position

          case acc.direction do
            :east ->  %{acc | position: {x+1, y}}
            :south -> %{acc | position: {x, y-1}}
            :west ->  %{acc | position: {x-1, y}}
            :north -> %{acc | position: {x, y+1}}
          end
      end)
    else
      {:error, "invalid instruction"}
    end

  end

  defp rr(:north), do: :east
  defp rr(:east), do: :south
  defp rr(:south), do: :west
  defp rr(:west), do: :north

  defp rl(dir), do: dir |> rr |> rr |> rr

  @doc """
  Return the robot's direction.

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  @spec direction(robot :: any) :: atom
  def direction(%RobotSimulator{direction: dir}) do
    dir
  end

  @doc """
  Return the robot's direction.

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  @spec position(robot :: any) :: { integer, integer }
  def position(%RobotSimulator{position: pos}) do
    pos
  end
end
