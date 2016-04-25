class School() {
  var db = collection.immutable.TreeMap[Int, Seq[String]]()

  def add(value: String, key: Int): Unit =  {
    val ls = db.getOrElse(key, Seq())
    db = db + (key -> (ls :+ value))
  }

  def grade(key: Int) = db.getOrElse(key, Seq())

  def sorted = db.map { case (k, v) => k -> v.sorted }
}
