def prefix_sum(a)
  n = a.length
  ps = [0] * (n + 1)

  for k in 1..n
    ps[k] = ps[k-1] + a[k-1]
  end
  ps
end

def prefix_sum2(a)
  r = []
  acc = 0
  a.each_with_index do |n, idx|
    acc += n
    r[idx] = acc
  end
  r
end


a = 0..10
r = prefix_sum(a.to_a)

r2 = prefix_sum2(a.to_a)
puts r.inspect
puts r2.inspect
