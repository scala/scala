object O {
  implicit def i: Int = 0
}

import O._

trait Foo {
  implicit val v1: Any
  implicit def d1: Any
           val v2: Any
  implicit val v3: Any
}

trait Bar1 extends Foo {
  implicit val v1      = {implicitly[Int]; ()} // failed due to cycle in Context#implicits being broken with Nil.
           def d1      = {implicitly[Int]; ()} // okay
  implicit val v2      = {implicitly[Int]; ()} // okay
  implicit val v3: Any = {implicitly[Int]; ()} // okay

}
