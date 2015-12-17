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

  // inplace reverse
  def reverse[T](head: Node[T]): Node[T] = {
    var newHead: Option[Node[T]] = None

    def traverse(current: Node[T], prev: Option[Node[T]]): Unit = {
      current.next match {
        case Some(next) => traverse(next, Option(current))
        case None => newHead = Option(current)
      }

      current.next = prev
    }

    traverse(head, None)
    newHead.get
  }

  // inplace reverse
  // 1 -> 2 -> 3 -> 4 -> 5
  def reverse2[T](head: Node[T]): Node[T] = {
    var limit = 10
    var next: Option[Node[T]] = head.next
    var prev: Option[Node[T]] = None
    var current = head

    var doNext = true
    while(doNext || limit > 0) {
      if (next.isEmpty) doNext = false
      limit -= 1
    }

    current
  }

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
val ints = Node.fromSeq(1, 2, 3, 4, 5)
Node.reverse(ints)
val ints2 = Node.fromSeq(1, 2, 3, 4, 5)
Node.reverse2(ints2)

Node.isEqual(
  Node.fromSeq('g', 'e', 'e', 'k', 's', 'a'),
  Node.fromSeq('g', 'e', 'e', 'k', 's', 'b'))

Node.isEqual(
  Node.fromSeq('g', 'e', 'e', 'k', 's', 'a'),
  Node.fromSeq('g', 'e', 'e', 'k', 's'))

Node.isEqual(
  Node.fromSeq('g', 'e', 'e', 'k', 's'),
  Node.fromSeq('g', 'e', 'e', 'k', 's'))


