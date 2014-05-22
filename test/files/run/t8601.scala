object Test {
  def idiv(x: Int): Unit = x / 0
  def ldiv(x: Long): Unit = x / 0
  def irem(x: Int): Unit = x % 0
  def lrem(x: Long): Unit = x % 0

  def check(x: => Any) = try { x; sys.error("failed to throw divide by zero!") } catch { case _: ArithmeticException => }

  def main(args: Array[String]) {
    check(idiv(1))
    check(ldiv(1L))
    check(irem(1))
    check(lrem(1L))
  }
}
