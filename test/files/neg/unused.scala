//> using options -Werror -Wunused:patvars

case class C(a: Int, b: Int, c: Int, d: Int)

object Test {
  val patvars = C(1, 2, 3, 4) match {
    case C(x, 2, _, _) => 1
    case C(2, b, y@_, _) => 2
    case C(a, b@_, c, d@_) => 3
    case e => 4
  }
}
