require 'Set'

module Pangram
  def self.is_pangram?(str)
    Set.new(str.downcase.scan(/[a-z]/)).length > 25
  end
end
