# http://www.codeforces.com/contest/610/problem/A

class Solv
  def initialize(limit)
    @limit = limit
    @count = 0
  end

  def run
    target = @limit / 2

    @count = (target/2-1)
    @count = 0 if @count < 0
    @count += 1 if (target % 2 != 0) and target != 1
    @count = 0 if @limit %2 != 0

    @count
  end
end

len = gets.to_i
solv = Solv.new(len)
puts solv.run
