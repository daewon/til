defmodule DNA do
  @nucleotides [?A, ?C, ?G, ?T]

  @doc """
  Counts individual nucleotides in a DNA strand.

  ## Examples

  iex> DNA.count('AATAA', ?A)
  4

  iex> DNA.count('AATAA', ?T)
  1
  """
  @spec count([char], char) :: non_neg_integer
  def count(strand, nucleotide) do
    case histogram(strand) do
      %{^nucleotide => count} -> count
      _ -> raise ArgumentError
    end
  end


  @doc """
  Returns a summary of counts by nucleotide.

  ## Examples

  iex> DNA.histogram('AATAA')
  %{?A => 4, ?T => 1, ?C => 0, ?G => 0}
  """
  @spec histogram([char]) :: Map
  def histogram(strand) do
    init = %{ ?A => 0, ?T => 0, ?C => 0, ?G => 0 }
    merged = Enum.reduce(strand, init, fn ch, acc ->
      Map.merge(acc, %{ch => 1}, fn _k, a, b -> a + b end)
    end)

    map_size(merged) !== 4 and raise ArgumentError
    merged
  end
end
