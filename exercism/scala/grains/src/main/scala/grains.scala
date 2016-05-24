object Grains {
  def square(n: Int): BigInt = BigInt(2).pow(n-1)
  def total: BigInt = (1 to 64).map(square).sum
}
