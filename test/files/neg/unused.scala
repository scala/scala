//> using options -Wunused:patvars -Werror

case class C(a: Int, b: Int, c: Int, d: Int)

object Test {
  val patvars = C(1, 2, 3, 4) match {
    case C(a, _, _, _) => 1
    case C(_, b, c@_, _) => 2
    case C(a, b@_, c, d@_) => 3
    case e => 4
  }
}