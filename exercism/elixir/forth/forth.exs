defmodule Forth do
  defmodule Eval do
    defstruct stack: []

    def eval(ev, ops) do
      ops = ev.stack ++ ops
      ret = do_eval(ops, [], %{})
      %Eval{stack: ret}
    end

    def do_eval([], ret, _), do: ret |> Enum.reverse
    def do_eval([h], [], _), do: [h]
    def do_eval([h|t]=stack, pops, env) do
      cond do
        udf?(h) ->
          {name, ops} = parse_udf(h)
          cond do
            Map.has_key?(env, name) ->
              cmds = Map.get(env, name)
              IO.inspect(cmds)
            true -> do_eval(t, pops, Map.put(env, name, ops))
          end
        over?(h) ->
          [a, b|pt] = pops
          do_eval(t, [b, a, b|pt], env)
        swap?(h) ->
          [a, b|pt] = pops
          do_eval(t, [b, a|pt], env)
        drop?(h) ->
          [_|pt] = pops
          do_eval(t, pt, env)
        dup?(h) ->
          [ph|_] = pops
          do_eval(t, [ph|pops], env)
        bin_op?(h) ->
          [b, a |pt] = pops
          ret = eval_bin(h, a, b) |> Integer.to_string
          do_eval(t, [ret|pt], env)
        number?(h) ->
          do_eval(t, [h|pops], env)
      end
    end

    defp parse_udf(ch) do
      [name|ops] = String.split(ch, [":", ";", ",", " "], trim: true)
      {name, ops}
    end

    defp udf?(ch) do
      ch = String.strip(ch)
      String.at(ch, 0) == ":" and String.last(ch) == ";"
    end

    defp over?(ch) do
      ch in ["OVER"]
    end

    defp swap?(ch) do
      ch in ["SWAP"]
    end

    defp drop?(ch) do
      ch in ["DROP"]
    end

    defp dup?(ch) do
      ch in ["DUP"]
    end

    defp bin_op?(ch) do
      ch in ["+", "-", "*", "/"]
    end

    defp number?(ch) do
      Regex.match?(~r/\d/, ch)
    end

    defp eval_bin(op, na, nb) do
      [a, b] = [na, nb] |> Enum.map(&String.to_integer/1)
      case op do
        "+" -> a + b
        "-" -> a - b
        "*" -> a * b
        "/" ->
          if b == 0, do: raise Forth.DivisionByZero
          div(a, b)
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
    inputs =
      Regex.scan(
        ~r/\d|[A-Z]+\-?[A-Z]+|DUP|DROP|SWAP|OVER|[\/*+-]|:.*?;/,
        String.upcase(s))
        |> List.flatten

    if hd(inputs) == "DUP" and length(inputs) == 1, do: raise Forth.StackUnderflow
    IO.puts("\n=============================")
    IO.inspect(inputs)

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
