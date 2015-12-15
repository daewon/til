//Input:
//  list1 = g->e->e->k->s->a
//  list2 = g->e->e->k->s->b
//Output: -1
//
//Input:
//  list1 = g->e->e->k->s->a
//  list2 = g->e->e->k->s
//Output: 1
//
//Input:
//  list1 = g->e->e->k->s
//  list2 = g->e->e->k->s
//Output: 0

case class Node[T](value: T) {
  var next: Option[Node[T]] = None

  override def toString = {
    var ls = Vector(value)
    var next = this.next

    while (next.isDefined) {
      ls =  ls :+ next.get.value

      next = next.get.next
    }

    ls.mkString(" -> ")
  }
}

object Node {

  /**
    * @param a
    * @param b
    * @tparam T
    * @return
    */
  def append[T](a: Node[T], b: Node[T]) = {
    a.next = Option(b)
    b
  }

  def fromSeq[T](value: T*) = {
    var head: Node[T] = null
    var current: Node[T] = null

    value.foreach { v =>
      if (current == null) {
        head = Node(v)
        current = head
      } else current = append(current, Node(v))
    }

    head
  }

  def isEqual(a: Node[Char], b: Node[Char]): Boolean = {
    val isEqual = a.value == b.value

    if (a.next.isDefined && b.next.isDefined)
      isEqual && Node.isEqual(a.next.get, b.next.get)
    else isEqual && a.next == b.next
  }
}

Seq(1) == Seq(1)

Node.isEqual(
  Node.fromSeq('g', 'e', 'e', 'k', 's', 'a'),
  Node.fromSeq('g', 'e', 'e', 'k', 's', 'b'))

Node.isEqual(
  Node.fromSeq('g', 'e', 'e', 'k', 's', 'a'),
  Node.fromSeq('g', 'e', 'e', 'k', 's'))

Node.isEqual(
  Node.fromSeq('g', 'e', 'e', 'k', 's'),
  Node.fromSeq('g', 'e', 'e', 'k', 's'))


