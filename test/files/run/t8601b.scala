object Test {
  def len(x: Array[String]): Unit = x.length
  def load(x: Array[String]): Unit = x(0)
  def newarray(i: Int): Unit = new Array[Int](i)

  def check(x: => Any) = try { x; sys.error("failed to throw NPE!") } catch { case _: NullPointerException => }
  def checkNegSize(x: => Any) = try { x; sys.error("failed to throw NegativeArraySizeException!") } catch { case _: NegativeArraySizeException => }

  def main(args: Array[String]) {
    check(len(null)) // bug: did not NPE
    check(load(null))
    checkNegSize(newarray(-1))
  }
}
