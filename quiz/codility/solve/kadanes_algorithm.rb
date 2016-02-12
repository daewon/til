a = [5, 7, 12, 0, -8, 6]
b = [6, -2, -3, 4, -1, 10]
c = [-1, -3, 4, -7, 0, 2]
d = [1, -5, 2, -1, 3]

def n3(a)
  len = a.length
  max = -1

  (0...len).each do |i|
    sum = 0
    (i...len).each do |j|
      sum += a[j]
      max = [max, sum].max
    end
  end

  max
end

def dp(a)
  # Two variables to store results
  # max_ending_here = max_so_far = 0

  # for i = 0 to n-1
  #     max_ending_here = max(a[i], max_ending_here + a[i])
  #     max_so_far = max(max_so_far, max_ending_here)
  # return max_so_far

  len = a.length
  max_ending_here, max_so_far = a[0], a[0]
  a[1...len].each do |n|
    max_ending_here = [n, max_ending_here + n].max
    max_so_far = [max_so_far, max_ending_here].max
  end

  max_so_far
end

puts n3(a)
puts n3(b)
puts n3(c)
puts n3(d)

puts dp(a)
puts dp(b)
puts dp(c)
puts dp(d)
