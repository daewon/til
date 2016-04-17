defmodule Forth do
  @opaque evaluator :: %{}
  defstruct stack: [], env: %{}

  alias Forth, as: F

  @doc """
  Create a new evaluator.
  """
  def new() do
    built_in = %{
      "+" => fn [b, a | t], _ -> [a + b | t] end,
      "-" => fn [b, a | t], _ -> [a - b | t] end,
      "*" => fn [b, a | t], _ -> [a * b | t] end,
      "/" => fn [b, a | t], _ ->
        if b == 0, do: raise F.DivisionByZero; [div(a, b) | t]
      end,
      "DUP" => fn
        [], _ -> raise Forth.StackUnderflow
        [a | t], _ -> [a, a | t]
      end,
      "DROP" => fn
        [], _ -> raise Forth.StackUnderflow
        [_ | t], _ -> t
      end,
      "SWAP" => fn
        [], _ -> raise Forth.StackUnderflow
        [_], _ -> raise Forth.StackUnderflow
        [a, b | t], _ -> [b, a| t]
      end,
      "OVER" => fn
        [], _ -> raise Forth.StackUnderflow
        [_], _ -> raise Forth.StackUnderflow
        [a, b | t], _ -> [b, a, b| t]
      end
    }

    %F{env: built_in}
  end

  @spec parse_input(String.t) :: [String.t]
  def parse_input(s) do
    s = String.upcase(s)

    if udf?(s) do
      cmds = String.split(s, ";", trim: true)
      cmds |> Enum.map(&String.strip/1)
    else
      Regex.split(~r/[^\p{L}\p{N}\p{S}\p{P}]+/u, s)
    end
  end

  @spec eval(evaluator, String.t) :: evaluator
  def eval(ev, s) do
    parsed_input = s |> parse_input
    parsed_input |> Enum.reduce(ev, &do_eval/2)
  end

  defp number?(expr), do: Regex.match?(~r/^\d+$/, expr)
  defp udf?(expr), do: Regex.match?(~r/:.*/, expr)

  defp parse_udf(expr) do
    [name| cmds] = String.split(expr, [":", " ", ";"], trim: true)

    if number?(name), do: raise F.InvalidWord

    func = fn _, ev ->
      Enum.reduce(cmds, ev, fn cmd, acc ->
        do_eval(cmd, acc)
      end).stack
    end

    {name, func}
  end

  def do_eval(expr, %F{stack: stack, env: env}=ev) do
    func = env[expr]
    cond do
      number?(expr) -> %F{ev | stack: [String.to_integer(expr)|stack]}
      is_function(func) -> %F{ev | stack: func.(stack, ev)}
      udf?(expr) ->
        {name, func} = parse_udf(expr)
        %F{ev | env: Map.merge(env, %{name => func})}
      true -> raise F.UnknownWord
    end
  end

  @spec format_stack(evaluator) :: String.t
  def format_stack(ev) do
    ev.stack |> Enum.reverse |> Enum.join(" ")
  end

  defmodule StackUnderflow do
    defexception [:message]
    def exception(_), do: %StackUnderflow{message: "stack underflow"}
  end

  defmodule InvalidWord do
    defexception [:message]
    def exception(w), do: %InvalidWord{message: "invalid word: #{inspect w}"}
  end

  defmodule UnknownWord do
    defexception [:message]
    def exception(w), do: %UnknownWord{message: "unknown word: #{inspect w}"}
  end

  defmodule DivisionByZero do
    defexception [:message]
    def exception(_), do: %DivisionByZero{message: "division by zero"}
  end
end
