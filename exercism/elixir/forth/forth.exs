defmodule Forth do
  defmodule Eval do
    defstruct stack: [], env: %{}

    def eval(ev, ops) do
      ops = ev.stack ++ ops
      {ret, new_env} = do_eval(ops, [], ev.env)
      %Eval{stack: ret, env: new_env}
    end

    def do_eval([], ret, env), do: {ret |> Enum.reverse, env}
    def do_eval([h], [], env), do: {[h], env}
    def do_eval([h|t]=stack, pops, env) do
      cond do
        udf?(h) ->
          {name, ops} = parse_udf(h)
          do_eval(t, pops, Map.put(env, name, ops))
        over?(h, env) ->
          [a, b|pt] = pops
          do_eval(t, [b, a, b|pt], env)
        swap?(h, env) ->
          [a, b|pt] = pops
          do_eval(t, [b, a|pt], env)
        drop?(h, env) ->
          [_|pt] = pops
          do_eval(t, pt, env)
        dup?(h, env) ->
          [ph|_] = pops
          do_eval(t, [ph|pops], env)
        bin_op?(h) ->
          [b, a |pt] = pops
          ret = eval_bin(h, a, b) |> Integer.to_string
          do_eval(t, [ret|pt], env)
        number?(h) ->
          do_eval(t, [h|pops], env)
        true ->
          {name, ops} = parse_udf(h)
          if Map.has_key?(env, name) do
            cmds = Map.get(env, name)
            do_eval(t ++ cmds, pops, env)
          end
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

    defp over?(ch, env) do
      ch in ["OVER"] and !Map.has_key?(env, ch)
    end

    defp swap?(ch, env) do
      ch in ["SWAP"] and !Map.has_key?(env, ch)
    end

    defp drop?(ch, env) do
      ch in ["DROP"] and !Map.has_key?(env, ch)
    end

    defp dup?(ch, env) do
      ch in ["DUP"] and !Map.has_key?(env, ch)
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

    # IO.puts("\n=============================")
    # IO.inspect(inputs)

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
    defexception [:message]
    def exception(_), do:  %DivisionByZero{message: "division by zero"}
  end
end
