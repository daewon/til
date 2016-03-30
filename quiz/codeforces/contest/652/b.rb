gets # ignore
sorted = gets.split(" ").map(&:to_i).sort
lo, hi = 0, sorted.length - 1

ret = []
while lo < hi
  ret << sorted[lo]
  ret << sorted[hi]

  lo += 1
  hi -= 1
end
ret << sorted[hi] if lo == hi

puts ret.join(" ")
