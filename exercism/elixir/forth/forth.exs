defmodule Forth do
  defmodule Eval do
    defstruct stack: []

    def eval(ev, ops) do
      ops = ev.stack ++ ops |> Enum.reverse
      do_eval(ops, [])

      %Eval{stack: ops |> Enum.reverse}
    end

    def do_eval([h|t]=stack, pops) do
      # IO.inspect(stack)
      IO.inspect(pops)
      cond do
        h in ["+", "-"] -> do_eval(t, [h|pops])
        true ->
          [ph|pt] = pops
          cond do
            ph in ["+", "-"] -> do_eval(t, [h|pops])
            true ->
              sum = String.to_integer(h) + String.to_integer(ph)
              op = Enum.take(pt, 1)
              rest = Enum.drop(pt, 1)
              do_eval([sum | t], pt)
          end
      end
    end
  end

  @opaque evaluator :: any

  @doc """
  Create a new evaluator.
  """
  @spec new() :: evaluator
  def new() do
    %Eval{}
  end

  @doc """
  Evaluate an input string, updating the evaluator state.
  """
  @spec eval(evaluator, String.t) :: evaluator
  def eval(ev, s) do
    inputs = s
    |> String.downcase
    |> String.split("", trim: true)
    |> Enum.filter(fn ch ->
      Regex.match?(~r/\d|[+-]/, ch)
    end)

    Eval.eval(ev, inputs)
  end

  @doc """
  Return the current stack as a string with the element on top of the stack
  being the rightmost element in the string.
  """
  @spec format_stack(evaluator) :: String.t
  def format_stack(ev) do
    ev.stack |> Enum.join(" ")
  end

  defmodule StackUnderflow do
    defexception []
    def exception(_), do: "stack underflow"
  end

  defmodule InvalidWord do
    defexception [:word]
    def exception(e), do: "invalid word: #{inspect e.word}"
  end

  defmodule UnknownWord do
    defexception [:word]
    def exception(e), do: "unknown word: #{inspect e.word}"
  end

  defmodule DivisionByZero do
    defexception []
    def exception(_), do: "division by zero"
  end
end
