# you can write to stdout for debugging purposes, e.g.
# puts "this is a debug message"

def solution(a)
    sum = a.inject(0) { |acc, curr| acc + curr }    
    size = a.length+1
    ((1 + size) * size / 2) - sum
end

