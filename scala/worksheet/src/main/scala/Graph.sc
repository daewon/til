import scala.collection._

/**
  * Scala, Traversing a graph in functional way
  * http://manuel.kiessling.net/2016/02/15/scala-traversing-a-graph-in-a-functional-way/
  */

case class Vertex(name: String, edges: Set[Vertex])

def dfsMutableIterative(start: Vertex): Set[Vertex] = {
  var current: Vertex = start
  val found: mutable.Set[Vertex] = mutable.Set[Vertex]()
  val stack: mutable.Stack[Vertex] = mutable.Stack[Vertex]()

  stack.push(current)

  while (stack.nonEmpty) {
    current = stack.pop()

    if (!found.contains(current)) {
      found += current

      for (next <- current.edges) stack.push(next)
    }
  }

  found.toSet
}

// How can we solve this functional way?
def dfsMutableRecursive(start: Vertex): Set[Vertex] = {
  val found: mutable.Set[Vertex] = mutable.Set[Vertex]()

  def recursive(current: Vertex): Unit = {
    found += current

    for (next <- current.edges) {
      if (!found.contains(next)) {
        recursive(next)
      }
    }
  }

  recursive(start)
  found.toSet
}

def dfsFunctional(current: Vertex, acc: Set[Vertex]): Set[Vertex] = {
  val ret = for {
    next <- current.edges if !acc.contains(next)
    child <- dfsFunctional(next, acc + next)
  } yield child

  ret + current
}

def dfsFold(current: Vertex, acc: Set[Vertex]): Set[Vertex] =
  current.edges.foldLeft(acc + current) { case (acc, curr) =>
    if (acc.contains(curr)) acc
    else acc + curr
  }

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


