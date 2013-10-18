import scala.language.implicitConversions

// the classes involved
case class Z[U](a: U)
case class Intermediate[T, U](t: T, u: U)
class Implicit1[T](b: Implicit2[T])
class Implicit2[T](c: Implicit3[T])
class Implicit3[T](/* and so on */)

object Test extends App {
  // the base conversion
  implicit def convertToZ[T](a: A[T])(implicit b: Implicit1[T]): Z[A[T]] = Z(a)

  // and the implicit chaining, don't you just love it? :D
  // implicit1, with one alternative
  implicit def implicit1[T <: Intermediate[_, _]](implicit b: Implicit2[T])                = new Implicit1[T](b)
  // implicit2, with two alternatives
  implicit def implicit2alt1[T <: Intermediate[_ <: String, _]](implicit c: Implicit3[T])  = new Implicit2[T](c)
  implicit def implicit2alt2[T <: Intermediate[_ <: Double, _]](implicit c: Implicit3[T])  = new Implicit2[T](c)
  // implicit3, with two alternatives
  implicit def implicit3alt1[T <: Intermediate[_, _ <: Int]]                               = new Implicit3[T]()
  implicit def implicit3alt2[T <: Intermediate[_ <: Double, _ <: AnyRef],X]                = new Implicit3[T]()

  // and our targets
  /** conversion here, with constraints */
  class A[T]()

  (new A).a
  (new A[Nothing]).a
}
