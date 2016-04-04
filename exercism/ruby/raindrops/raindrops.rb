class Raindrops
  MAP = {3 => 'Pling', 5 => 'Plang', 7 => 'Plong'}

  def self.convert(n)
    mapped = factorize(n).map { |n| MAP[n] }.compact
    mapped.empty? ? n.to_s : mapped.join("")
  end

  def self.factorize(n)
    primes, i, limit = [], 2, n

    while i <= limit
      if n % i == 0
        primes << i
        n = n / i
      else
        i = i + 1
      end
    end

    primes.uniq
  end
end
