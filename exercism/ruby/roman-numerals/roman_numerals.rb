module RomanNumerals
  ROMAN_NUMERAL_MAP = {
    1000 => "M", 900 => "CM", 500 => "D", 400 => "CD",
    100  => "C", 90  => "XC", 50  => "L", 40  => "XL",
    10   => "X", 9   => "IX", 5   => "V", 4   => "IV",
    1 => "I"
  }

  def to_roman
    number = self
    romans = []
    noms = ROMAN_NUMERAL_MAP.keys

    while number > 0 do
      n = noms.first

      if number < n
        noms = noms.drop(1)
      else
        romans << ROMAN_NUMERAL_MAP[n]
        number -= n
      end
    end

    romans.join("")
  end
end

class Integer
  VERSION = 1

  include RomanNumerals
end
