module Year
  def self.leap?(y)
    if y % 400 == 0
      true
    elsif y % 100 == 0
      false
    elsif y % 4 == 0
      true
    else
      false
    end
  end
end
