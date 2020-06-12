//scalac: -Yimports:java.lang

import scala.Int

object H1 {
  case class StringContext(i: Int)

  val x = 3
  val str = s"a: $x"
}
object H2 {
  case class StringContext(i: Int*)

  val x = 3
  val str = s"a: $x"
}
object H3 {
  case class StringContext(strs: String*) {
    def s: Int = 3
  }

  val x = 3
  val str = s"a: $x"
}
object X1 {
  case class StringContext(strs: String*) {
    def s(args: Int*): String = strs.mkString + args.sum
  }
  val x = 3
  val str = s"a: $x"
  val bad = s"b: $y"
}
object X2 {
  case class StringContext(strs: String*) {
    def s(args: Int*): String = strs.mkString + args.sum
  }
  val x = 3
  val bad = t"a: $x"
}
object X3 {
  case class StringContext(strs: String*) {
    def s(args: String*): String = scala.StringContext(strs: _*).s(args: _*)
  }
  val x = 3
  val bad = s"a: $x"
}
