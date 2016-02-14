coin = -> n, divs, i, memo do
  return 1 if n == 0
  return 0 if n < 0 or i == -1

  memo[n] = [] if memo[n].nil?
  return memo[n][i] unless memo[n][i].nil?

  memo[n][i] = coin.call(n, divs, i-1, memo) + coin.call(n - divs[i], divs, i, memo)
end

# 110
# [10, 50, 100, 500]
def coin2(n, coins)
  memo = (0..n).map { |i| 0 }
  memo[0] = 1
  coins.each do |coin|
    (0..n).each do |i|
      memo[i + coin] += memo[i] if (i + coin <= n)
    end
  end
  memo[n]
end

gets.to_i.times do
  n, count_of_coins = gets.split(" ").map &:to_i
  coins = gets.split(" ").map &:to_i
  # puts coin.call(n, coins, coins.length - 1, []) % 1000000007 
  puts coin2(n, coins) % 1000000007
end

