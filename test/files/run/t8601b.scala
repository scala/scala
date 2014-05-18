object Test {
  def len(x: Array[String]): Unit = x.length

  def check(x: => Any) = try { x; sys.error("failed to throw NPE!") } catch { case _: NullPointerException => }

  def main(args: Array[String]) {
    check(len(null))
  }
}
