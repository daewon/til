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
