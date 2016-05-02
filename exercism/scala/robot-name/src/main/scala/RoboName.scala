object Robot {
  import scala.util.Random
  def randomCh = ('A'.toInt + Random.nextInt(26)).toChar
  def randomDigit = Random.nextInt(10)

  def generateName(numOfChar: Int = 2, numOfDigit: Int = 3): String = {
    val chPart = (0 until numOfChar).map(_ => randomCh).mkString("")
    val digitPart = (0 until numOfDigit).map(_ => randomDigit).mkString("")

    chPart + digitPart
  }
}

class Robot() {
  var name: String = Robot.generateName()
  def reset(): Unit = name = Robot.generateName()
}
