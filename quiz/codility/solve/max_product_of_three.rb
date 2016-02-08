# you can write to stdout for debugging purposes, e.g.
# puts "this is a debug message"

def solution(a)
  len = a.length
  sorted = a.sort
  plus, minus = sorted.partition { |a| a > 0 }

  a = plus[(plus.length-3)..(plus.length-1)] || []
  b = minus[0..1] || []

  # case 1
  max = sorted[(len-3)..(len-1)].inject(:*)

  # case 2
  if b.length == 2 && a.length > 0
    sum = b.inject(:*) * a[-1]
    max = sum if sum > max
  end

  max
end
