object Deque {
  case class Node[T](
    var value: T,
    var prev: Node[T] = null,
    var next: Node[T] = null
  )

  def apply[T](): Deque[T] = new Deque[T]()
}

class Deque[T] {
  import Deque._

  var count = 0
  var head: Node[T] = null
  var last: Node[T] = null

  def push(arg: T) = last match {
    case null =>
      val node = Node(arg)

      last = node
      head = node

      count += 1
    case _ =>
      val node = Node(arg)

      node.prev = last
      last.next = node
      last = node

      count += 1
  }

  def pop(): Option[T] = last match {
    case null => None
    case _ =>
      val value = last.value
      val prev = last.prev

      if (prev != null) {
        prev.next = null
        last = prev
      }

      count -= 1
      checkSize()

      Option(value)
  }

  def shift(): Option[T] = head match {
    case null => None
    case _ =>
      val value = head.value
      val next = head.next

      if (next != null) {
        next.prev = null
        head = next
      }

      count -= 1
      checkSize()

      Option(value)
  }

  def unshift(arg: T): Unit = head match {
    case null =>
      val node = Node(arg)

      head = node
      last = node

      count += 1
    case _ =>
      val node = Node(arg)

      node.next = head
      head.prev = node
      head = node

      count += 1
  }

  private def checkSize(): Unit = {
    if (count == 0) {
      head = null
      last = null
    }
  }
}
