defmodule Wordy do

  @doc """
  Calculate the math problem in the sentence.
  """
  @spec answer(String.t) :: integer
  def answer(question) do
    code = question
    |> String.replace("What is", "")
    |> String.replace("plus", "+")
    |> String.replace("minus", "-")
    |> String.replace("multiplied by", "*")
    |> String.replace("divided by", "/")
    |> String.replace("?", "")

    code
    |> String.split(" ", trim: true)
    |> do_eval
  end

  def do_eval([acc]), do: acc
  def do_eval(terms) do
    {exp, rest} = Enum.split(terms, 3)
    evaluated = eval_exp(exp)

    do_eval([evaluated | rest])
  end

  defp eval_exp([a, op, b]) when is_binary(a), do: eval_exp([String.to_integer(a), op, b])
  defp eval_exp([a, op, b]) when is_binary(b), do: eval_exp([a, op, String.to_integer(b)])

  defp eval_exp([a, "+", b]), do: a + b
  defp eval_exp([a, "-", b]), do: a - b
  defp eval_exp([a, "*", b]), do: a * b
  defp eval_exp([a, "/", b]), do: a / b
end
