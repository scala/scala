
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
