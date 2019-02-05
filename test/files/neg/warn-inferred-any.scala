// scalac: -Xfatal-warnings -Xlint:infer-any
//
trait Foo[-A <: AnyRef, +B <: AnyRef] {
  def run[U](x: A)(action: B => U): Boolean = ???

  def foo = { run(_: A)(_: B => String) }
}

trait Xs[+A] {
  { List(1, 2, 3) contains "a" }  // only this warns
  { List(1, 2, 3) contains 1 }
  { identity(List(1, 2, 3) contains 1) }
  { List("a") foreach println }
}

trait Ys[+A] {
  { 1 to 5 contains 5L }
  { 1L to 5L contains 5 }
  { 1L to 5L contains 5d }
  { 1L to 5L contains 5L }
}

trait Zs {
  def f[A](a: A*) = 42
  def g[A >: Any](a: A*) = 42  // don't warn

  def za = f(1, "one")
  def zu = g(1, "one")
}

class C1
class C2

trait Cs {
  val cs = List(new C1)
  cs.contains[AnyRef](new C2) // doesn't warn
  cs.contains(new C2) // warns
}
