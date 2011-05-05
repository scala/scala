class Crash {
  def S(op: => Double) = 0
  def A(a: Int, b: Int) = 0

  val t = 0

  val q = A(
    b = S { val xxx = t ; 42 },
    a = 0
  )
}
