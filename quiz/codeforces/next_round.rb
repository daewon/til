_, k = gets.split(" ").map(&:to_i)
arr = gets.split(" ").map(&:to_i)

arr = arr.select { |n| n >= arr[k-1] }
arr = arr.reject { |n| n <= 0 }

puts arr.length
