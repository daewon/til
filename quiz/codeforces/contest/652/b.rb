gets # ignore
arr = gets.split(" ").map(&:to_i)
arr = arr.zip_with_index

puts arr
