object O {
  def foo[T](t: T) = 0
  def foo(s: String)(implicit i: DummyImplicit = null) = 1
}

object Test extends App {
  assert(O.foo("") == 1)
}
