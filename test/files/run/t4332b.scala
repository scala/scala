object Test extends App {
  def check(expected: Any, actual: Any, msg: String = "") = {
    if (expected != actual)
      sys.error(s"($actual != $expected) $msg")
  }
  val ls = List(1, 3, 2, 1)
  for (N <- -1 to (ls.length + 1)) {
    check(ls.takeRight(N), ls.view.takeRight(N).toList, s"takeRight($N)")
    check(ls.dropRight(N), ls.view.dropRight(N).toList, s"dropRight($N)")
  }
  for (N <- 1 to (ls.length + 1)) {
    check(ls.sliding(N).toList, ls.view.sliding(N).toList.map(_.toList), s"sliding($N)")
    check(ls.sliding(N, 2).toList, ls.view.sliding(N, 2).toList.map(_.toList), s"sliding($N, 2)")
  }
  for (b <- List(true, false))
    check(ls.filterNot(x => true), ls.view.filterNot(x => true), s"filterNot($b)")

  check(ls.inits.toList, ls.view.inits.toList.map(_.toList), "inits")
  check(ls.tails.toList, ls.view.tails.toList.map(_.toList), "tails")

  check(ls.combinations(2).toList.map(_.toList), ls.view.combinations(2).toList.map(_.toList), "combinations(2)")
  check(ls.permutations.toList.map(_.toList), ls.view.permutations.toList.map(_.toList), "permutations")

  check(ls.sortBy(_ * -1), ls.view.sortBy(_ * -1).toList, "sortBy")
  check(ls.sortWith((x, y) => y < x), ls.view.sortWith((x, y) => y < x).toList, "sortWith")
  check(ls.sorted, ls.view.sorted.toList, "sorted")

  check(ls.distinct, ls.view.distinct.toList, "distinct")

  check(ls.tail, ls.view.tail.toList, "tail")  

  import collection.mutable.Buffer
  check(Buffer(1, 2, 3).tail, Buffer(1, 2, 3).view.tail.toList, "Buffer#tail")  
  check(Buffer(1, 2, 3).tail.length, Buffer(1, 2, 3).view.tail.length, "Buffer#tail#length")  
}
