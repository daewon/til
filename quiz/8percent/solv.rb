# https://brunch.co.kr/@sunghokimnxag/5

def has_seven(n)
  return true if n % 7 == 0

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

def pingpong2(n)
  pad = 1
  nums = iterate([1, 1]) do |idx, acc|
    pad *= -1 if has_seven(idx)

    [idx + 1, acc + pad]
  end

  nums.take(n).last[1]
end


def pingpong(n)
  @name = 10
  def get_increment(idx)
    return 1 if idx == 1

    if has_seven(idx-1)
      get_increment(idx-1) * -1
    else
      get_increment(idx-1)
    end
  end

  def pingpong_rec(n)
    return 1 if n == 1

    pingpong_rec(n-1) + get_increment(n)
  end

  pingpong_rec(n)
end

puts pingpong(8) # 6
puts pingpong(22) # 0
puts pingpong(68) # 2
puts pingpong(100) # 2
puts pingpong2(5000) # -26
puts pingpong(5000) # -26
