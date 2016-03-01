# you can write to stdout for debugging purposes, e.g.
# puts "this is a debug message"

# [2, 1, 5, 1, 2, 2, 2]
def solution(k, m, a)
  min = 999999999999999

  trav = -> a, i, j do
    if i + j < a.length
      t1 = a[0...i].inject(0) { |a, c| a + c }
      t2 = a[i...j].inject(0) { |a, c| a + c }
      t3 = a[j...a.length].inject(0) { |a, c| a + c }

      # puts [t1, t2, t3].inspect
      min = [min, [t1, t2, t3].max].min
      trav.call(a, i+1,j)
      trav.call(a, i, j+1)
    end
  end

  trav.call(a, 0, 0)

  min
end
