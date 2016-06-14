class Phrase
  def initialize(s)
    @sentence = s.downcase
  end

  def word_count
    @sentence.scan(/[a-z0-9]+[']?[a-z]+|\d+/).flatten.each_with_object({}) do |word, map|
      if map[word]
        map[word] += 1
      else
        map[word] = 1
      end
    end
  end
end
