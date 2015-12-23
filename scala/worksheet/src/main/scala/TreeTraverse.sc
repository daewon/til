import scala.annotation.tailrec

case class Node[T](value: T, childNodes: Node[T]*) {
  override def toString = value.toString
}

def traverse[T](parents: List[Node[T]]): List[List[Node[T]]] = {
  val childNodes = parents.flatMap(_.childNodes)

  if (childNodes.isEmpty) List(parents)
  else parents :: traverse(childNodes)
}

def traverse2[T](parents: List[Node[T]]): List[List[Node[T]]] = parents match {
  case Nil => Nil
  case _ =>
    val childNodes = for {
      parent <- parents
      childNode <- parent.childNodes
    } yield childNode

    parents :: traverse2(childNodes)
}

def traverse3[T](root: Node[T]): List[List[Node[T]]] = {
  var lss: List[List[Node[T]]] = Nil

  @tailrec def loop(parents: List[Node[T]]) {
    lss = parents :: lss
    val childNodes = parents.flatMap(_.childNodes)

    if (childNodes.nonEmpty) loop(childNodes)
  }

  loop(List(root))
  lss.reverse
}

def traverse4[T](root: Node[T]): List[List[Node[T]]] = {
  var lss: List[List[Node[T]]] = Nil

  var parents = List(root)
  while (parents.nonEmpty) {
    lss = parents :: lss

    val childs = for {
      parent <- parents
      node <- parent.childNodes
    } yield node

    parents = childs
  }

  lss.reverse
}

@tailrec def traverse5[T](parents: List[Node[T]], acc: List[List[Node[T]]] = Nil): List[List[Node[T]]] = {
  val childNodes = parents.flatMap(_.childNodes)

  if (childNodes.isEmpty) acc
  else traverse5(childNodes, childNodes :: acc)
}

def levelOrder[T](root: Node[T]) = traverse(List(root))
def levelOrder2[T](root: Node[T]) = traverse2(List(root))
def levelOrder3[T](root: Node[T]) = traverse3(root)
def levelOrder4[T](root: Node[T]) = traverse4(root)
def levelOrder5[T](root: Node[T]) = traverse5(List(root), List(List(root))).reverse
val root = Node(1,
  Node(2,
    Node(3,
      Node(4,
        Node(5)))),
  Node(2,
    Node(3))
)
val ls = levelOrder(root)
val ls2 = levelOrder2(root)
val ls3 = levelOrder3(root)
val ls4 = levelOrder4(root)
val ls5 = levelOrder5(root)
ls.size == 5
ls(0).size == 1
ls(1).size == 2
ls(2).size == 2
ls(3).size == 1
ls(4).size == 1

