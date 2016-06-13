module Grains
  def self.square(n)
    i = 1
    (n-1).times { i = i * 2 }
    i
  end

  def self.total
    (1..64).inject(0) { |acc, n| square(n) + acc }
  end
end
