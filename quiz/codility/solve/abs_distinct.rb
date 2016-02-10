# you can write to stdout for debugging purposes, e.g.
# puts "this is a debug message"

def solution(a)
  len = a.length

  i, j, count = 0, len - 1, 0
  while i <= j do
    l, r = a[i].abs, a[j].abs

    if l < r
      while j > 0 and r == a[j].abs
        j -= 1
      end
    elsif l > r
      while i < len and l == a[i].abs
        i += 1
      end
    elsif l == r
      while j > 0 and r == a[j].abs
        j -= 1
      end

      while i < len and l == a[i].abs
        i += 1
      end
    end

    count += 1
  end

  count
end
