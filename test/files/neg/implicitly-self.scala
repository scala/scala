//> using options -Werror -Xlint:implicit-recursion
//

trait TC[A] { def ix: Int }

object Test {
  implicit def c: Char = implicitly[Char]
  implicit val s: String = implicitly[String]
  implicit val t: Int = {
    def f = implicitly[Int]
    f
  }
  implicit object tcString extends TC[String] { def ix = implicitly[TC[String]].ix + 1 }
}

import language.implicitConversions

trait T
trait Sizeable { def size: Int }

class `t8357 warn on self-involved implicit` {
  implicit def bad[A](a: A)(implicit ev: A => T): Sizeable = ev(a)
  bad(new T{})
}
