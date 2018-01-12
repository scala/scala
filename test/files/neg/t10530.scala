class X(val u: Any with X) extends AnyVal
class Y(val u: Y with Y) extends AnyVal
class Z(val u: Z with String) extends AnyVal
class U(val u: U with Int) extends AnyVal

class W(val u: Z with U) extends AnyVal
class R(val u: Z {}) extends AnyVal

class Q(val u: AnyRef with X) extends AnyVal

class A(val a: Int) extends AnyVal
class B[T <: A](val a: T) extends AnyVal