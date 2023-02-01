
// scalac: -Werror -Xsource:3

class C
class D

object Test extends App {
  val c = new C
  val d = new D
  d match {
    case `c` => true
    case _ => false
  }
}
