def solve(t, acc, type)
  return -1 if t == 0 and @n_mul >= @d_mul and acc < @goal

  loop do
    if type == :day
      return t if acc >= @goal
      t = t + 1
      acc = acc + @d_mul * 12
      type = :night
    else
      acc = acc - @n_mul * 12
      type = :day
    end
  end
end

sp, @goal = gets.split(" ").map(&:to_i)
@d_mul, @n_mul = gets.split(" ").map(&:to_i)

puts solve(0, sp + (8 * @d_mul), :day)
