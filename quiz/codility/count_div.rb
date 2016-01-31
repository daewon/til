# you can write to stdout for debugging purposes, e.g.
# puts "this is a debug message"

def solution(a, b, k)
  # write your code in Ruby 2.2
  e = b / k
  s = (a-1) / k

  e - s
end
