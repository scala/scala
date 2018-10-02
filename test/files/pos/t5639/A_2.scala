// scalac: -Xsource:2.12
import Implicits._

class Baz

object Test {
  implicitly[Int]
}

object Implicits  {
  implicit val Baz: Int = 0
}

/*
 * under -Xsource:2.11
 *
  A_2.scala:6: error: could not find implicit value for parameter e: Int
  implicitly[Int]
            ^
one error found
 */
