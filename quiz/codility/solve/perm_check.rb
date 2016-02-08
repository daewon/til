def solution(a)
  # write your code in Ruby 2.2
  return a.first == 1 ? 1 : 0 if a.one?

  set = {}
  sum, max, min = 0, -1, 9999999999999999999

  a.each do |n|
    return 0 if set.include? n

    set[n] = true
    max = n if n > max
    min = n if n < min
    sum += n
  end

  (sum == (min + max) * max / 2) ? 1 : 0
end
