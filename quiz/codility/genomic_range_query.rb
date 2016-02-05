# you can write to stdout for debugging purposes, e.g.
# puts "this is a debug message"

def solution(s, p, q)
  # write your code in Ruby 2.2
  chm = { 'A' => 1, 'C' => 2, 'G' => 3, 'T' => 4 }
  acc = { 'A' => 0, 'C' => 0, 'G' => 0, 'T' => 0 }
  map = { 'A' => [], 'C' => [], 'G' => [], 'T' => [] }

  # make prefix_sum
  s.each_char.with_index do |ch, i|
    n = chm[ch]
    acc[ch] += n
    chm.keys.each { |c| map[c][i] = acc[c] }
  end

  # puts map.inspect

  # detect minimal charectre in range(p, q)
  p.zip(q).map do |i, j|
    next chm[s[i]] if i == j

    c = chm['T']
    chm.keys.each do |ch|
      lhs = (i-1 >= 0) ? map[ch][i-1] : 0
      if lhs < map[ch][j]
        c = chm[ch]
        break
      end
    end
    c
  end
end
