object Bearing {
  trait Dir
  case object North extends Dir
  case object East extends Dir
  case object West extends Dir
  case object South extends Dir
}

case class Robot(bearing: Bearing.Dir, coordinates: (Int, Int)) {
  import Robot._
  import Bearing._

  def advance = bearing match {
    case North => copy(coordinates = (coordinates._1, coordinates._2 + 1))
    case East => copy(coordinates = (coordinates._1 + 1, coordinates._2))
    case South => copy(coordinates = (coordinates._1, coordinates._2 - 1))
    case West => copy(coordinates = (coordinates._1 - 1, coordinates._2))
  }

  def turnRight() = bearing match {
    case North => copy(bearing = East)
    case East => copy(bearing = South)
    case South => copy(bearing = West)
    case West => copy(bearing = North)
  }

  def turnLeft() = turnRight.turnRight.turnRight

  def simulate(in: String) = in.foldLeft(this) {
    case (acc, 'A') => acc.advance
    case (acc, 'L') => acc.turnLeft
    case (acc, 'R') => acc.turnRight
  }
}
