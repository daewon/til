# https://brunch.co.kr/@sunghokimnxag/5

def has_seven(n)
  while n > 0
    d = n % 10
    return true if d == 7

    n = n / 10
  end

  false
end

def iterate(init, &block)
  prev = init
  enum = Enumerator.new do |yielder|
    while true
      yielder << prev
      prev = block.call(prev)
    end
  end

  enum
end

def pingpong(n)
  pad = 1
  nums = iterate([1, 1]) do |idx, acc|
    pad *= -1 if has_seven(idx) || idx % 7 == 0

    [idx + 1, acc + pad]
  end

  nums.take(n).last[1]
end

puts pingpong(8) # 6
puts pingpong(22) # 0
puts pingpong(68) # 2
puts pingpong(100) # 2
