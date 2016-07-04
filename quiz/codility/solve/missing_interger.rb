# you can write to stdout for debugging purposes, e.g.
# puts "this is a debug message"

def solution(a)
  num_map = a.each_with_object({}) do |n, acc|
    acc[n] = n
  end
  
  remains = (1..100001).drop_while do |n|
    num_map[n]
  end
  
  remains.first || 0
end
