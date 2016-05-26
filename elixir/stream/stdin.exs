IO.stream(:stdio, :line)
|> Stream.map(&String.strip/1)
|> Stream.with_index
|> Stream.map(fn {line, i} -> "#{i}: #{line}" end)
|> Enum.take(1)
|> IO.inspect
