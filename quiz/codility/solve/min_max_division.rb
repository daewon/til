# you can write to stdout for debugging purposes, e.g.
# puts "this is a debug message"

def solution(k, m, a)
  len = a.length
  ret = 9999999999999999

  def traverse(a, i, j, depth)
    j = i + 1

    t1 = traverse(a, 0+1, 0, 0)
    t2 = traverse(a, 0+1, 0, 0)
    t3 = traverse(a, 0, 0+1, 0)
  end

  traverse(a, 0, 0, 0)
end
