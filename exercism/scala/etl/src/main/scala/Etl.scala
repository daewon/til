object ETL {
  def transform(from: Map[Int, Seq[String]]): Map[String, Int] =
    from.flatMap { case (k, words) =>
      words.map { word => word.toLowerCase -> k }
    }
}
