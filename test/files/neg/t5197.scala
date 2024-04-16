
//> using options -Werror -feature

// Periodic reminder that the feature is not required for implicit function values.
//import scala.language.implicitConversions

object A {
  val x: Int = List("a")
  implicit lazy val O1: (List[String] => Int) = _.head.toInt
}
object B {
  val y: Int = List("b")
  implicit object O2 extends (List[String] => Int) { def apply(ss: List[String]): Int = ss.head.toInt }
}
