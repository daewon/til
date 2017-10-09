package io.daewon.util

trait LRUCache {
  type Key
  type Value

  val capacity: Int

  def size: Int
  def get(key: Key): Option[Value]
  def set(key: Key, value: Value): this.type
  def remove(key: Key): Boolean
}

object DoubleLinkedList {
  case class Node[A](var item: A, var prev: Node[A], var next: Node[A]) {
    override def toString = s"{ item: ${item}, next: \n${next} }"
  }
}

class DoubleLinkedList[A]() {
  import DoubleLinkedList._

  private[util] var head: Node[A] = _
  private[util] var last: Node[A] = _

  def moveToHead(node: Node[A]): Boolean = {
    this.remove(node)
    this.add(node)

    true
  }

  def removeLast(): Node[A] = {
    val old = this.last
    this.remove(this.last)

    old
  }

  def +=(node: Node[A]): Boolean = this.add(node)

  def add(node: Node[A]): Boolean = {
    node.next = null
    node.prev = null

    if (this.head == null) {
      this.head = node
      this.last = node
    } else {
      this.head.prev = node
      node.next = this.head
      this.head = node
    }

    true
  }

  def -=(node: Node[A]): Boolean = this.remove(node)

  def remove(node: Node[A]): Boolean = {
    if (head == null || (node == this.head && this.head == this.last)) {
      return false
    }

    if (node == this.head) {
      this.head.next.prev = null
      this.head = this.head.next

    } else if (node == this.last) {
      this.last.prev.next = null
      this.last = this.last.prev

    } else { // node has prev and next nodes
      node.prev.next = node.next
      node.next.prev = node.prev

    }

    true
  }
}

class SimpleLRUCache[A, B](val capacity: Int) extends LRUCache {
  require(capacity > 0)

  import scala.collection._
  import DoubleLinkedList._

  type Key = A
  type Value = B

  case class KV(key: Key, value: Value)

  def size: Int = data.size
  def isFull: Boolean = data.size == capacity
  def isEmpty: Boolean = data.size == 0

  private[util] val data = mutable.Map.empty[Key, Node[KV]]
  private[util] val list = new DoubleLinkedList[KV]()

  def get(key: Key): Option[Value] = {
    if (!data.contains(key)) None
    else {
      val node = data(key)
      this.list.moveToHead(node)

      Option(node.item.value)
    }
  }

  def set(key: Key, value: Value): this.type = {
    if (this.data.contains(key)) {
      val node = this.data(key)
      this.list.moveToHead(node)
      node.item = KV(key, value)

    } else {
      if (this.data.size == this.capacity) {
        val lastNode = this.list.removeLast()
        this.data -= lastNode.item.key
      }

      val node = Node(KV(key, value), null, null)

      this.list += node
      this.data += key -> node
    }

    this
  }

  def remove(key: Key): Boolean = {
    if (data.contains(key)) {
      this.data -= key
      this.list -= data(key)
    } else {
      false
    }
  }
}
