
/**
This is a tricky issue which has to do with the fact that too much conflicting
type information is propagated into a single implicit search, where the intended
solution applies two implicit searches.

Roughly, in x + x * y, the first x is first typed as Poly[A]. That
means the x * y is then typed as Poly[A]. Then the second x is typed
as Poly[A], then y is typed as Poly[Poly[A]]. The application x * y
fails, so the coef2poly implicit conversion is applied to x. That
means we look for an implicit conversion from type Poly[A] to type
?{val *(x$1: ?>: Poly[Poly[A]] <: Any): Poly[A]}. Note that the result
type Poly[A] is propagated into the implicit search. Poly[A] comes as
expected type from x+, because the lhs x is still typed as a Poly[A].
This means that the argument of the implicit conversion is typechecked
with expected type A with Poly[A]. And no solution is found.

To solve this, I added a fallback scheme similar to implicit arguments:
When an implicit view that adds a method matching given arguments and result
type fails, try again without the result type.
*/
trait Ring[T <: Ring[T]] {
  def +(that: T): T
  def *(that: T): T
}

class A extends Ring[A] {
  def +(that: A) = new A
  def *(that: A) = new A
}

class Poly[C <: Ring[C]](val c: C) extends Ring[Poly[C]] {
  def +(that: Poly[C]) = new Poly(this.c+that.c)
  def *(that: Poly[C]) = new Poly(this.c*that.c)
}

object Test extends App {

  implicit def coef2poly[C <: Ring[C]](c: C): Poly[C] = new Poly(c)

  val a = new A
  val x = new Poly(new A)

  println(x+a) // works
  println(a+x) // works

  val y = new Poly(new Poly(new A))

  println(x+y*x) // works
  println(x*y+x) // works
  println(y*x+x) // works

  println(x+x*y) // failed before
}
