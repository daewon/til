# you can write to stdout for debugging purposes, e.g.
# puts "this is a debug message"

def solution(a)
  max = [-9999, -9999, -9999]
  min = [9999, 9999]

  a.each do |n|
    if n > max[0]
      max[1] = max[0]
      max[2] = max[1]
      max[0] = n
    elsif n > max[1]
      max[2] = max[1]
      max[1] = n
    elsif n > max[2]
      max[2] = n
    end

    if n < min[0]
      min[1] = min[0]
      min[0] = n
    elsif n < min[1]
      min[1] = n
    end
  end
  puts max.inspect
  puts min.inspect

  sorted = (max + min).reject { |n| n.abs == 9999 }.sort
  puts sorted.inspect

  a = sorted[0..2].inject(:*) * sorted[-1]
  b = (sorted[(sorted.length-3)..(sorted.length-1)] || []) .inject(:*)

  [a, b].max
end

