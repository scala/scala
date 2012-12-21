class AnnotNotFound {
  def foo(a: Any) = ()

  foo {
    @inargument
    def foo = 0
    foo
  }

  () => {
    @infunction
    def foo = 0
    ()
  }

  () => {
    val bar: Int = {
      @nested
      val bar2: Int = 2
      2
    }
    ()
  }

  def func(@param x: Int): Int = 0

  abstract class A {
    @typealias
    type B = Int
  }

  @classs
  class C

  @module
  object D

  class D[@typeparam T]

  class E(
    @valueparam x: Any
  )
}
