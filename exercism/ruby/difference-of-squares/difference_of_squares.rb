class Squares
  def initialize(n)
    @n, @rng = n, 1..n
  end

  def square_of_sum
    sum_of_square = @rng.inject(:+)
    sum_of_square * sum_of_square
  end

  def sum_of_squares
    @rng.map { |n| n * n }.inject(:+)
  end

  def difference
    return 0 if @n == 0
    square_of_sum - sum_of_squares
  end
end
