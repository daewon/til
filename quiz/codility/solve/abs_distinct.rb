# you can write to stdout for debugging purposes, e.g.
# puts "this is a debug message"

def solution(a)
  uniq = a.uniq { |n| n.abs }
  uniq.length
end
