# you can write to stdout for debugging purposes, e.g.
# puts "this is a debug message"

def solution(n, a)
  counter = [0] * n
  counter_max, max = 0, 0

  a.each do |c|
    i = c-1

    if c == n + 1
      counter_max = max
    else
      # should ensure start number after max_counter applied
      counter[i] = counter_max if counter[i] < counter_max
      counter[i] += 1

      max = [max, counter[i]].max
    end
  end

  # should ensure start number after max_counter applied
  counter.map { |c| [c, counter_max].max }
end
