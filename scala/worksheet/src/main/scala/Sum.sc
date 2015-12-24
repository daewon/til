// Quiz: make possible sum

def possibleSum(goal: Int) = {
  var acc: List[List[Int]] = Nil

  def traverse(curr: Int, path: List[Int]) {
    lazy val ls = curr to goal

    if (path.sum > goal) Nil
    else if (path.sum == goal) acc = path :: acc
    else ls.foreach { n =>
      traverse(n, n :: path)
    }
  }

  traverse(1, Nil)
  acc
}

def possibleSum2(goal: Int) = {
  def traverse(start: Int, sum: Int, path: List[Int]): Seq[List[Int]] = {
    lazy val ls = start to goal

    if (sum > goal) Nil
    else if (sum == goal) List(path)
    else ls flatMap { n =>
      traverse(n, sum + n, n :: path)
    }
  }

  traverse(1, 0, Nil)
}

possibleSum(1)
possibleSum(2)
possibleSum(3)
possibleSum(4)
possibleSum(5)

possibleSum2(1)
possibleSum2(2)
possibleSum2(3)
possibleSum2(4)
possibleSum2(5)
