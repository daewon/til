module Complement
  MAP = { "G" => "C", "C" => "G", "T" => "A", "A" => "U" }

  def self.of_dna(ch)
    mapped = ch.each_char.map do |ch|
      raise ArgumentError unless MAP[ch]
      MAP[ch]
    end

    mapped.join("")
  end
end
