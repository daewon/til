import scala.collection._

case class Edge(label: String, src: Int, tgt: Int)

val sc = mutable.SortedSet.empty[Edge](new Ordering[Edge] {
  override def compare(x: Edge, y: Edge): Int = {
    val ord = implicitly[Ordering[(String, Int, Int)]]
    ord.compare((x.label, x.src, x.tgt), (y.label, y.src, y.tgt))
  }
})
sc.add(Edge("talk", 0, 1))
sc.add(Edge("talk", 0, 2))
sc.add(Edge("talk", 0, 3))
sc.add(Edge("talk", 1, 0))
sc.add(Edge("talk", 1, 1))
sc.add(Edge("agit", 0, 1))
sc.add(Edge("agit", 0, 2))
sc.add(Edge("agit", 0, 3))
sc.add(Edge("agit", 1, 0))
sc.add(Edge("agit", 1, 1))
sc.toList
sc.range(Edge("talk", 0, 0), Edge("talk", 0, 1000)).toList

sc.from(Edge("talk", 0, 0)).slice(0, 3)

val it = sc.iteratorFrom(Edge("talk", 0, 1))
it.take(2).toList


