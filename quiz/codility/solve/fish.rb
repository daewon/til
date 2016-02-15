# you can write to stdout for debugging purposes, e.g.
# puts "this is a debug message"

def solution(a, b)
  stack = []
  a.zip(b).each do |size, dir|
    stack << [size, dir]
  end

  alive = 0
  # puts stack.inspect
  pops = []
  loop do
    if pops.empty?
      curr = stack.pop
      if curr[1] == 1
        alive += 1
      else
        pops << curr
      end
    else
      if pops.last[1] == stack.last[1]
        pops << stack.pop
      else
        if pops.last[0] > stack.last[0]
          stack.pop
        else
          pops.pop
        end
      end
    end

    break if stack.empty?
  end

  # puts stack.inspect
  # puts pops.inspect

  stack.size + pops.size + alive
end
