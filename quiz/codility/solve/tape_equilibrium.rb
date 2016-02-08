# you can write to stdout for debugging purposes, e.g.
# puts "this is a debug message"

def solution(a)
  # write your code in Ruby 2.2
  sum = a.inject(:+)
  acc = 0

  min = 99999999
  a[0..-2].each do |n|
    sum -= n
    acc += n

    min = [(acc - sum).abs, min].min
  end
  min
end
