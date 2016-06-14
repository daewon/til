class Phrase
  def initialize(s)
    @sentence = s
  end
  def word_count
    map = {}
    @sentence.(/[:\s]/).each do |word|
      if map[word]
        map[word] += 1
      else
        map[word] = 1
      end
    end

    map
  end
end
