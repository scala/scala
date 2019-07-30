
import annotation.implicitNotFound

@implicitNotFound("foo")
trait TC[A] {
  type B
}

@implicitNotFound("bar")
trait XC[A] {
  type B
}

@implicitNotFound("nope")
trait T

trait U extends T

@implicitNotFound("no way")
trait V extends T

object Example {
  implicitly[TC[String] { type Int}]
  implicitly[XC[String]]
  implicitly[U]
  implicitly[V]

  def f(implicit v: V) = ???
  def g(implicit @implicitNotFound("huh") v: V) = ???

  f
  g
}

@implicitNotFound("No F of ${A}")
trait F[A]

trait M[A] extends F[A]

trait AX extends F[String]

@implicitNotFound("Missing X3 of ${A} and ${B} and ${C}")
trait X3[A, B, C]
trait X2[A, B] extends X3[A, B, String]
trait X1[A] extends X2[A, Int]
trait X0 extends X1[Char]

object SuperSubstitutions {
  implicitly[F[Int]]
  implicitly[M[Int]]
  implicitly[AX]
  implicitly[X0]
}
