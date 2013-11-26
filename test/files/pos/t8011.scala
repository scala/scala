class ThingOps1(val x: String) extends AnyVal {
  def fn[A]: Any = {
    new X[A] { def foo(a: A) = a }
    0
  }
}

trait X[B] { def foo(a: B): Any }