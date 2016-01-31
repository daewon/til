# you can write to stdout for debugging purposes, e.g.
# puts "this is a debug message"

def solution(a)
  acc = 0
  sum = 0

  a.each do |n|
    if n == 0
      acc += 1
    else
      sum += acc
    end

    return -1 if sum > 1000000000
  end

  sum
end
