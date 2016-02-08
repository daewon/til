# you can write to stdout for debugging purposes, e.g.
# puts "this is a debug message"

def solution(n, a)
  counter = [0] * n
  max, last_max = 0, 0

  a.each do |k|
    if k >= 1 and k <= n
      counter[k-1] = last_max if counter[k-1] < last_max
      counter[k-1] += 1

      max = counter[k-1] if counter[k-1] > max
    elsif k == n + 1
      last_max = max
    end
  end

  counter.each_with_index do |_, i|
    counter[i] = last_max if counter[i] < last_max
  end
end
