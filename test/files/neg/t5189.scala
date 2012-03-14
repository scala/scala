class TestNeg1 {
  case class Foo[T, U](f: T => U)
  def f(x: Any): Any => Any = x match { case Foo(bar) => bar }
  // uh-oh, Any => Any should be Nothing => Any.
}
