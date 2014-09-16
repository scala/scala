trait Foo[-A <: AnyRef, +B <: AnyRef] {
  def run[U](x: A)(action: B => U): Boolean = ???

  { run(_: A)(_: B => String) }
}

trait Xs[+A] {
  { List(1, 2, 3) contains "a" }  // only this warns
  { List(1, 2, 3) contains 1 }
  { identity(List(1, 2, 3) contains 1) }
  { List("a") foreach println }
}

trait Ys[+A] {
  { 1 to 5 contains 5l }
  { 1l to 5l contains 5 }
  { 1l to 5l contains 5d }
  { 1l to 5l contains 5l }
}

trait Zs {
  def f[A](a: A*) = 42
  def g[A >: Any](a: A*) = 42  // don't warn

  def za = f(1, "one")
  def zu = g(1, "one")
}
