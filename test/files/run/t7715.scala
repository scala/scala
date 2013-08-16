
import PartialFunction.cond
import util._

object Test extends App {

  object I { def unapply(x: String): Option[Int] = Try(x.toInt).toOption }
  implicit class RX(val sc: StringContext) {
    def rx = sc.parts.mkString("(.+)").r
  }

  Console println ("2 by 4" match {
    case rx"${I(a)} by ${I(b)}" => a+b
    case _                      => -1
  })
  Console println ("2 by 4" match {
    case rx"${_} by ${I(b)}"    => b    // pattern placeholder
    case _                      => -1
  })
  Console println ("2 by 4" match {
    case rx"$_ by ${I(b)}"      => b    // is permitted this way, too
    case _                      => -1
  })
}
