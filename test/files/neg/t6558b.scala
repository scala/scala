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
}
