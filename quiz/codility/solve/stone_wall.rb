# you can write to stdout for debugging purposes, e.g.
# puts "this is a debug message"

def solution(h)
  len = h.length
  stack = []
  stone_num = 0

  h.each do |h|
    while !stack.empty? and stack.last > h
      stack.pop
    end

    if stack.last and stack.last > h
      stack.pop
    elsif stack.last and stack.last == h
    # pass
    else
      stack << h
      stone_num += 1
    end
  end

  stone_num
end
