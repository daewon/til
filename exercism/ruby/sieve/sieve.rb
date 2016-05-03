class Sieve
  def initialize(n)
    @limit = n
  end

  def primes
    return [] if @limit < 2

    range = 3..@limit
    [2] + range.step(2).select { |n| is_prime?(n) }
  end

  def is_prime?(n)
    i = 2
    while i <= Math.sqrt(n)
      return false if n % i == 0
      i += 1
    end

    true
  end
end
