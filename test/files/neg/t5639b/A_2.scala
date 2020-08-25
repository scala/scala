// scalac: -Xsource:2.11
import Implicits._

class Baz

object Test {
  implicitly[Int]
}

object Implicits  {
  implicit val Baz: Int = 0
}
