object Bearing {
  trait Dir
  case object North extends Dir
  case object East extends Dir
  case object West extends Dir
  case object South extends Dir
}

object Robot {
  import Bearing._
}

case class Robot(bearing: Bearing.Dir, coordinates: (Int, Int)) {
  import Robot._
  import Bearing._

  def advance = copy(bearing = bearing, coordinates = (coordinates._1, coordinates._2))
  def turnRight = ???
  def turnleft = ???
  def simulate(in: String) = ???
}
