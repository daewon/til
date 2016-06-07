import scala.annotation._

case class BankAccount() {
  var balance: Option[Int] = Option(0)
  @volatile var isOpen: Boolean = true

  def getBalance: Option[Int] = synchronized {
    if (isOpen) balance else None
  }

  def incrementBalance(amount: Int): Option[Int] = synchronized {
    if (isOpen) balance = balance.map(_ + amount)
    getBalance
  }

  def closeAccount(): Unit = isOpen = false
}
