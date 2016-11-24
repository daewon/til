# https://www.hackerrank.com/challenges/area-under-curves-and-volume-of-revolving-a-curv

as = [1, 2, 3, 4, 5]
bs = [6, 7, 8, 9, 10]

l = 1
r = 4

# t = (l..r).map do |x|
#   as.zip(bs).map do |a, b|
#     a * (x **j b.to_f)
#   end.inject(:+)
# end

# puts t
d=0.001
a = as
b = bs

p d * l.step(r, d).map{ |z| a.size.times.map { |i| a[i] * z ** b[i] }.reduce(:+) }.reduce(:+)
p d * Math::PI * l.step(r,d).map { |z| a.size.times.map { |i| a[i] * z ** b[i] }.reduce(:+) ** 2 }.reduce(:+)
