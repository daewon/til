class Integer
  VERSION = 1

  M = {
    1000 => "M", 900 => "CM", 500 => "D", 400 => "CD",
    100  => "C", 90  => "XC", 50  => "L", 40  => "XL",
    10   => "X", 9   => "IX", 5   => "V", 4   => "IV",
    1 => "I"
  }

  def to_roman
    number = self
    romans = []

    noms = M.keys
    while number > 0 do
      n = noms.first

      if number < n
        noms = noms.drop(1)
      else
        romans << M[n]
        number -= n
      end
    end

    romans.join("")
  end
end

class RomanNumerals < Integer

end

class String
  # Considers string a roman numeral numeral,
  # and converts it to the corresponding integer.
  def to_i_roman
    RomanNumerals.to_integer(self)
  end
  # Returns true iif the subject is a roman numeral.
  def is_roman_numeral?
    RomanNumerals.is_roman_numeral?(self)
  end
end
