class Robot
  def initialize()
    @name = generate_name
  end

  def name
    @name
  end

  def generate_name
    ch_part = [*('A'..'Z')].sample(2).join
    digit_part = Random.new.rand(1000)
    "#{ch_part}#{digit_part}"
  end

  def reset
    @name = generate_name
  end
end
