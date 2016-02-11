# you can write to stdout for debugging purposes, e.g.
# puts "this is a debug message"

def solution(a)
  stack = []

  a.each_with_index do |n, i|
    if stack.empty?
      stack << [i, n]
      next
    end

    last_value = stack.last[1]

    if n == last_value
      stack << [i, n]
    else
      stack.pop
    end
  end

  return -1 if stack.empty?

  i, cd = stack.last

  count = a.inject(0) do |acc, n|
    cd == n ? acc + 1 : acc
  end

  count > a.length / 2 ? i : -1
end
