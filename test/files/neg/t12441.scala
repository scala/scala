
//> using options -Werror -Xlint

trait T {
  def k(u: => Unit): Unit = u
  def f: Any = Option.empty[String].contains(1234)
  def g = Option.empty[String].contains(1234)
  def h() = k(Option.empty[String].contains(1234))

  def s0 = List(1, 2, 3).toSet()  // adapt mistaken arg () to apply and infer AnyVal

  val p = (42, 27)
  def pesky = p == (42, 17)
  def peskier = p == (42, List(17, ""))
}
