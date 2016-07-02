# you can write to stdout for debugging purposes, e.g.
# puts "this is a debug message"

def solution(x, a)
  # write your code in Ruby 2.2
  map = {}
  i = 0
  while i < a.length do
    n = a[i]
    map[n] = true

    break if map.length == x

    i += 1
  end

  if i < a.length
    i
  else
    -1
  end
end

# you can write to stdout for debugging purposes, e.g.
# puts "this is a debug message"
require 'set'
def solution(x, a)
  set = Set.new
  
  a.each_with_index do |n, i|
    set << n
    return i if set.length == x
  end
  
  -1
end
