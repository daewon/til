import scala.collection.mutable

case class Node(var value: Int, var next: Option[Node])

// 123 + 999
implicit class StringOps(s: String) {
  def toLs[Int](f: String => Int) = {
    val chars = s.split("").reverse
    var initNode = Node(chars.head.toInt, None)

    chars.tail.foreach { case ch =>
      initNode = Node(ch.toInt, Option(initNode))
    }

    initNode
  }
}
def addTwoLs[Int](a: Node, b: Node): Node = {
  val stackA: mutable.Stack[Node] = mutable.Stack()
  val stackB: mutable.Stack[Node] = mutable.Stack()

  var next = a.next
  while (next.isDefined) {
    stackA.push(next.get)
    next = a.next
  }

  next = b.next
  while (next.isDefined) {
    stackB.push(next.get)
    next = a.next
  }

  var carry = 0
  var nodeC: Node = Node(0, None: Option[Node])
  while (stackA.nonEmpty || stackB.nonEmpty) {
    if (stackA.nonEmpty && stackB.nonEmpty) {
      val valueA = stackA.pop.value
      val valueB = stackB.pop.value

      val sum = valueA + valueB + carry
      if (sum > 9) carry = 1
      else carry = 0

      nodeC = Node(sum % 10, None)
    } else if (stackA.nonEmpty && stackB.isEmpty) {
      nodeC = Node(stackA.pop().value, Option(nodeC))
    } else if (stackA.isEmpty && stackB.nonEmpty)
      nodeC = Node(stackB.pop().value, Option(nodeC))
    else {
      println("xx")

    }
  }

  nodeC
}
val lsA = "123".toLs(_.toInt)
val lsB = "99".toLs(_.toInt)

addTwoLs(lsA, lsB)
