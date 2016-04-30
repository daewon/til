object Robot {
  import scala.util.Random
  val chars = 'A' to 'Z' toSeq

  def generateName: String = {
    val twoCh = Random.shuffle(chars).take(2).mkString("")
    def num = Random.nextInt(10)
    s"$twoCh$num$num$num"
  }
}

class Robot() {
  var name: String = Robot.generateName
  def reset() = name = Robot.generateName
}
