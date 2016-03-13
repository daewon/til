defmodule DNA do
  @map %{?A => ?U, ?C => ?G, ?T => ?A, ?G => ?C}

  @doc """
  Transcribes a character list representing DNA nucleotides to RNA

  ## Examples

  iex> DNA.to_rna('ACTG')
  'UGAC'
  """
  @spec to_rna([char]) :: [char]
  def to_rna(dna) do
    Enum.map(dna, fn ch -> @map[ch] end)
  end
end
