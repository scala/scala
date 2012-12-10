class AnnotNotFound {
  def foo(a: Any) = ()

  foo {
    // Not yet issued in the context of this file, see SI-6758
    // This error is issued in t6558b.scala
    @inargument
    def foo = 0
    foo
  }

  () => {
    // As per above
    @infunction
    def foo = 0
    ()
  }

  @classs
  class C

  class D[@typeparam T]

  class E(
  	@valueparam x: Any
  )
}
