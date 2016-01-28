# https://codility.com/programmers/lessons/18/
# you can write to stdout for debugging purposes, e.g.
# puts "this is a debug message"

def solution(n)
  max = 0
  tmp = 0
  n.to_s(2).each_char do |s|
    if s == "0"
      tmp += 1
    else
      max = [tmp, max].max
      tmp = 0
    end
  end
  max
end
