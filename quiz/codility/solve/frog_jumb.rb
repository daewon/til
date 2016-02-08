# you can write to stdout for debugging purposes, e.g.
# puts "this is a debug message"

def solution(x, y, d)
  # write your code in Ruby 2.2
  return 0 if y == x

  dist = y - x

  if dist % d == 0
    dist / d
  else
    (dist / d) + 1
  end
end
