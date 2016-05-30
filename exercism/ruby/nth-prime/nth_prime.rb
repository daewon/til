class Prime
  def self.is_prime?(num)
    i = 2
    while i <= Math.sqrt(num)
      return false if num % i == 0
      i += 1
    end

    true
  end

  def self.nth(n)
    raise ArgumentError if n == 0
    sequence = Enumerator.new do |yielder|
      yielder.yield 2
      yielder.yield 3

      number = 5
      loop do
        yielder.yield number if is_prime?(number)
        number += 2
      end
    end

    nth = 0
    n.times { nth = sequence.next }
    nth
  end
end
