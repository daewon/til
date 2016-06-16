case class Position(x: Int, y: Int)

case class Queens(rows: Int = 8, cols: Int = 8) {
  def boardString(w: Option[Position], b: Option[Position]): String = {
    val board = for {
      row <- 0 until rows
      col <- 0 until cols
      pos = Option(Position(row, col))
    } yield pos match {
      case `w` => "W"
      case `b` => "B"
      case _ => "_"
    }

    board.grouped(8)
      .map(_.mkString(" "))
      .mkString("\n") + "\n"
  }

  def checkStraight(w: Position, b: Position) = w.x == b.x || w.y == b.y

  def checkDiagonal(w: Position, b: Position) = (w.x - b.x).abs == (w.y - b.y).abs

  def canAttack(w: Position, b: Position): Boolean = checkStraight(w, b) || checkDiagonal(w, b)
}
