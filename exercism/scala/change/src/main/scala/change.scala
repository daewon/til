object Change {
  def findFewestCoins(_amount: Int, _totalCoins: List[Int]): Option[List[Int]] = {
    def recur(remain: Int, coins: List[Int], used: List[Int]): List[Int] = {
      if (remain == 0) used
      else if (coins.isEmpty || remain < 0) Nil
      else {
        val coin :: remains = coins

        val coinUsed = recur(remain - coin, coins, coin :: used)
        val coinNotUsed = recur(remain, remains, used)

        (coinUsed, coinNotUsed) match {
          case (Nil, b) => b
          case (a, Nil) => a
          case (a, b) => if (a.length > b.length) b else a
        }
      }
    }

    if (_amount == 0) Some(Nil)
    else recur(_amount, _totalCoins, Nil) match {
      case Nil => None
      case coins => Some(coins.sorted)
    }
  }
}
