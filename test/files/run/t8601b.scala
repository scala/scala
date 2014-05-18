object Test {
  def len(x: Array[String]): Unit = x.length
  def load(x: Array[String]): Unit = x(0)

  def check(x: => Any) = try { x; sys.error("failed to throw NPE!") } catch { case _: NullPointerException => }

  def main(args: Array[String]) {
    check(len(null)) // bug: did not NPE
    check(load(null))
  }
}
