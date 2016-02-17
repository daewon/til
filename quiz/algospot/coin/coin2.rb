def solv(amount, idx)
  return 1 if amount == 0
  return 0 if amount < 0 or idx < 0

  @memo[amount] = [] if @memo[amount].nil?
  return @memo[amount][idx] if @memo[amount][idx]

  used = solv(amount - @coins[idx], idx)
  not_used = solv(amount, idx-1)

  @memo[amount][idx] = used + not_used
end

gets.to_i.times do
  amount, count_of_coins = gets.split(" ").map &:to_i
  coins = gets.split(" ").map &:to_i

  @memo = []
  @coins = coins
  puts solv(amount, coins.length-1) % 1000000007
end
