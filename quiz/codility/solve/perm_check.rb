# you can write to stdout for debugging purposes, e.g.
# puts "this is a debug message"

def solution(a)
  return 0 if a.uniq.size != a.size
  
  max = a.size  
  sum = (1 + max) * max / 2
  
  array_sum = a.inject(0, &:+)  
  sum == array_sum ? 1 : 0  
end
