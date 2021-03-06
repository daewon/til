defmodule Frequency do

  defmodule Worker do
    def do_calc(texts) do
      texts
      |> Enum.flat_map(&String.graphemes/1)
      |> Enum.map(&String.downcase/1)
      |> Enum.filter(fn ch -> Regex.match?(~r/\p{L}/, ch) end)
      |> Enum.group_by(&( &1 ))
      |> Enum.map(fn {k, ls} -> {k, length(ls)} end)
      |> Enum.into(%{})
    end
  end

  @doc """
  Count letter frequency in parallel.

  Returns a dict of characters to frequencies.

  The number of worker processes to use can be set with 'workers'.
  """
  @spec frequency([String.t], pos_integer) :: map
  def frequency(texts, workers) do
    texts
    |> chunk(workers)
    |> map
    |> await
    |> reduce
  end

  def chunk(texts, workers) do
    jobs = length(texts)
    cnt = div(jobs, workers)

    cond do
      jobs == 0 -> [[]]
      jobs > workers -> Enum.chunk(texts, cnt-1, cnt-1, [])
      true -> Enum.chunk(texts, jobs, jobs, [])
    end
  end

  def map(chunked_texts) do
    chunked_texts
    |> Enum.map(fn texts ->
      Task.async(Worker, :do_calc, [texts])
    end)
  end

  def await(pids) do
    pids
    |> Enum.flat_map(fn pid -> Task.await pid end)
  end

  def reduce(results) do
    results
    |> Enum.reduce(%{}, fn {k, c}, m ->
      Map.merge(m, %{k => c}, fn _k, o, n -> o + n end)
    end)
  end
end
