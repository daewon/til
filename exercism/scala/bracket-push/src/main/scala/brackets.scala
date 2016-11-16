object Brackets {
  val brackets: Map[Char, Char] = Map(
    '{' -> '}',
    '(' -> ')',
    '[' -> ']'
  )
  val bracketsClosed: Map[Char, Char] = brackets.map { case (k ,v) => v -> k }

  def areBalanced(_in: String): Boolean = {
    def traverse(in: String, stack: List[Char]): Boolean = in match {
      case "" => stack.isEmpty
      case ls if brackets.contains(ls.head) => traverse(in.tail, ls.head :: stack)
      case ls if bracketsClosed.contains(ls.head) =>
        if (stack.nonEmpty && bracketsClosed(ls.head) == stack.head) traverse(in.tail, stack.tail)
        else false
      case _ => traverse(in.tail, stack)
    }

    traverse(_in, Nil)
  }
}
