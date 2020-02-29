class A(val value: Int) extends AnyVal
class B(val value: Int) extends AnyVal

package vcArg {
  trait TC[T] { def v(x: A): T }
  object TC {
    implicit val tcB: TC[B] = _ => new B(0)
  }
}

package vcRes {
  trait TC[T] { def v(x: T): A }
  object TC {
    implicit val tcB: TC[B] = _ => new A(0)
  }
}

