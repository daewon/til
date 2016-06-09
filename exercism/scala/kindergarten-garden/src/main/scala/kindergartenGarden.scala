object Plant {
  trait PlantBase
  case object Radishes extends PlantBase
  case object Clover extends PlantBase
  case object Grass extends PlantBase
  case object Violets extends PlantBase

  val plantMap = Map("R" -> Radishes, "C" -> Clover, "G" -> Grass, "V" -> Violets)
}

object Garden {
  val defaultChildren = Seq(
    "Alice", "Bob", "Charlie", "David",
    "Eve", "Fred", "Ginny", "Harriet",
    "Ileana", "Joseph", "Kincaid", "Larry")

  def defaultGarden(s: String): Garden = Garden(defaultChildren, s)

  def apply(childrens: Seq[String], s: String): Garden =
    new Garden(s.split("\n"), childrens)
}

class Garden(in: Seq[String], childrens: Seq[String]) {
  import Garden._
  import Plant._

  val width = 2

  lazy val childrenMap = childrens.sorted.zipWithIndex.toMap

  def getPlants(name: String): Seq[PlantBase] =
    childrenMap.get(name).toSeq.flatMap { idx =>
      in.map(_.drop(idx * width)) // drop sz * idx per each line ex) "abcd" => "ab"
        .map(_.split("")) // "abcd" => Seq("a", "b", "c", "d")
        .map(_.map(plantMap(_))) // map Char to Plant ex)"G" => Grass
        .flatMap(_.take(width)) // take sz from head of list per each line
    }
}
