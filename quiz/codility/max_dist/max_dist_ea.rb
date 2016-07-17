# brute force
def solv1(arr)
  len = arr.size
  max_dist = 0

  for i in 0...len
    for j in 0...len
      if arr[i] != arr[j]
        max_dist = [j-i, max_dist].max
      end
    end
  end

  max_dist
end

def solv2(arr)
  len = arr.length
  max_dist = 0
  last_idx = len-1

  arr.each_with_index do |n, i|
    reverse_idx = last_idx - i

    if n != arr[last_idx]
      max_dist = [max_dist, reverse_idx].max
    end
  end

  max_dist
end

arr = [4, 6, 2, 2, 6, 6, 4]
arr = [4, 4, 4, 4, 4, 3, 4, 4]

puts solv1(arr)
puts solv2(arr)

def random_arr()
  n = (1..10).to_a.shuffle.first

  arr = []
  n.times do
    arr = arr + (1..1000).to_a.shuffle
  end
  arr
end

1000.times do
  arr = random_arr().shuffle
  puts "error" if solv1(arr) != solv2(arr)
end
