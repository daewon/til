# http://articles.leetcode.com/a-distance-maximizing-problem/

# brute force
def solv1(arr)
  len = arr.size
  dist_max = 0

  # `...` means exclusive range
  (0...(len - 1)).each do |i|
    max = 0
    ((i+1)...len).each do |j|
      if arr[j] > arr[i]
        max = j - i
      end
    end

    dist_max = max if max > dist_max
  end

  dist_max
end

def solv2(arr)
  sorted = arr.each_with_index.map.sort do |a, b|
    av, ai = a
    bv, bi = b

    if av != bv
      av <=> bv
    else
      ai <=> bi
    end
  end

  # puts "sorted: #{sorted.inspect}"

  len = arr.size
  with_table = []
  last_idx = 0
  m = {}
  for i in 0...len
    idx = (len - 1) - i
    v, i = sorted[idx]

    if i > last_idx
      if sorted[idx+1]
        prev_v, prev_i = sorted[idx+1]
        m[i] = last_idx
      end
      last_idx = i
    end

    with_table << [v, i, last_idx]
  end
  # puts "prev_map: #{m}"
  # puts "with_table: #{with_table.reverse.inspect}"

  max_dist = 0
  with_table.each do |v, oi, di|
    next if oi >= di

    if arr[di] == v
      prev_di  = m[di]
      next unless m[di]
      dist = prev_di - oi
    else
      dist = di - oi
    end

    max_dist = dist if dist > max_dist
  end

  max_dist
end

arr = [4, 3, 5, 2, 1, 3, 2, 3]
arr = [1, 2, 1]

puts solv1(arr)
puts solv2(arr)

def random_arr(n)
  n = Random.new.rand(n) + 1

  arr = [n, n, n]
  n.times do
    n2 = Random.new.rand(n) + 1
    n2.times do
      arr << Random.new.rand(n2) + 1
    end
  end

  arr
end


10000.times do
  n = Random.new.rand(50) + 1

  arr = random_arr(n)
  if solv1(arr) != solv2(arr)
    puts arr.inspect
  end
end
