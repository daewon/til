object CustomSet {
  type CS[A] = CustomSet[A]

  def fromList[A](ls: Seq[A]): CS[A] = new CustomSet[A](ls)

  def empty[A](set: CS[A]): Boolean = set.isEmpty

  def member[A](set: CS[A], item: A): Boolean = set.member(item)

  def isSubsetOf[A](a: CS[A], b: CS[A]): Boolean = a.isSubsetOf(b)

  def isDisjointFrom[A](a: CS[A], b: CS[A]): Boolean = a.disjointFrom(b)

  def isEqual[A](a: CS[A], b: CS[A]): Boolean = a.isEqual(b)

  def insert[A](a: CS[A], item: A): CS[A] = a.insert(item)

  def intersection[A](a: CS[A], b: CS[A]): CS[A] = a.intersection(b)

  def difference[A](a: CS[A], b: CS[A]): CS[A] = a.difference(b)

  def union[A](a: CS[A], b: CS[A]): CS[A] = a.union(b)
}

class CustomSet[A](ls: Seq[A]) {
  private var size = 0
  private val MaxSize = 10000
  private var data: Array[List[A]] = new Array(MaxSize)
  private case object BreakEx extends RuntimeException("break")

  // initial vlaues
  ls.foreach(insert)

  private def get(item: A): (Int, List[A]) = {
    val hashKey = hash(item)
    hashKey -> data(hashKey)
  }

  private def hash(item: A): Int = item.hashCode.abs % MaxSize

  def member(item: A): Boolean = {
    val (hashKey, oldLs) = get(item)

    if (oldLs == null) false
    else oldLs.contains(item)
  }

  def isSubsetOf(b: CustomSet[A]): Boolean = {
    var isSubset = true

    try {
      foreach { item =>
        if (!b.member(item)) throw BreakEx
      }
    } catch {
      case BreakEx => isSubset = false
    }

    isSubset
  }

  def disjointFrom(b: CustomSet[A]): Boolean = {
    var isDisjoint = true

    try {
      foreach { item =>
        if (b.member(item)) throw BreakEx
      }
    } catch {
      case BreakEx => isDisjoint = false
    }

    isDisjoint
  }

  def intersection(b: CustomSet[A]): CustomSet[A] = {
    val ret: CustomSet[A] = CustomSet.fromList(List())

    foreach { item =>
      if (b.member(item)) ret.insert(item)
    }

    ret
  }

  def difference(b: CustomSet[A]): CustomSet[A] = {
    val ret: CustomSet[A] = CustomSet.fromList(List())

    foreach { item =>
      if (!b.member(item)) ret.insert(item)
    }

    ret
  }

  def union(b: CustomSet[A]): CustomSet[A] = {
    val ret: CustomSet[A] = CustomSet.fromList(List())

    foreach { item =>
      ret.insert(item)
    }

    b.foreach { item =>
      ret.insert(item)
    }

    ret
  }

  def isEqual(b: CustomSet[A]): Boolean = length == b.length && isSubsetOf(b)

  def foreach(f: A => Unit) = data.foreach { items =>
    if (items != null) {
      items.foreach { item => f(item) }
    }
  }

  def length = size

  def isEmpty = length == 0

  def insert(item: A): CustomSet[A] = {
    val (hashKey, oldLs) = get(item)

    if (oldLs == null) {
      data(hashKey) = List(item)
      size += 1
    } else {
      if (!oldLs.contains(item)) {
        data(hashKey) = item :: oldLs
        size += 1
      }
    }

    this
  }

}
