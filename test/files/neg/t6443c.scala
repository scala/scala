trait A {
  type D >: Null <: C
  def foo(d: D)(a: Any, d2: d.type): Unit
  trait C {
    def bar: Unit = foo(null)(null, null)
  }
}
object B extends A {
  class D extends C

  def foo(d: D)(a: Any, d2: d.type): Unit = () // Bridge method required here!

  // No bridge method should be added, but we'll be happy enough if
  // the "same type after erasure" error kicks in before the duplicated
  // bridge causes a problem.
  def foo(d: D)(a: Any)(d2: d.type): Unit = ()
}

object Test extends App {
  new B.D().bar
}
