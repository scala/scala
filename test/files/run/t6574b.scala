object Test extends App {
  implicit class AnyOps(val i: Int) extends AnyVal {
    private def parentsOf(x: Int): List[Int] = if (x == 0) Nil else x :: parentsOf(x - 1)
    def parents: List[Int] = parentsOf(i)
  }
  println((5).parents)
}
