class Hamming
  VERSION = 1

  def self.compute(a, b)
    raise ArgumentError if a.length != b.length

    a.chars.zip(b.chars).inject(0) do |acc, tuple|
      a, b = tuple
      a == b ? acc : acc + 1
    end
  end
end
