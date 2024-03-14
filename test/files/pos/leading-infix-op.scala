//> using options -Xsource:3 -Xsource-features:leading-infix

trait T {
  def f(x: Int): Boolean =
    x < 0
    ||
    x > 0
    &&
    x != 3

  def g(x: Option[Int]) = x match {
    case Some(err) =>
      println("hi")
      ???
    case None =>
      ???
  }
}
