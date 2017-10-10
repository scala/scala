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

trait DefAny {
  def get(b: Boolean) = if (b) 42 else true // warn (AnyVal)
  def got(b: Boolean) = if (b) 42 else "42" // warn (Any)
}

trait ValAny {
  val foo = if (true) 42 else false // warn (AnyVal)
  val bar = if (true) 42 else "42" // warn (Any)
}

// these should not warn due to explicit types
trait ExplicitAny {
  def get(b: Boolean): AnyVal = if (b) 42 else true
  def got(b: Boolean): Any = if (b) 42 else "42"
  val foo: AnyVal = if (true) 42 else false
  val bar: Any = if (true) 42 else "42"
}
