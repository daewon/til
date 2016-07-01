# you can write to stdout for debugging purposes, e.g.
# puts "this is a debug message"

def solution(a)
  max = a.length + 1
  (1 + max) * max / 2 - a.inject(0, &:+)
end
