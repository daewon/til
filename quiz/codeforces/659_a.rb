n, a, b = gets.split(" ").map(&:to_i)
res = (b+a) % n

if res == 0
  puts n
else
  puts res
end
