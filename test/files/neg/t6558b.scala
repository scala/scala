class AnnotNotFound {
  def foo(a: Any) = ()

  foo {
    @inargument
    def foo = 0
    foo
  }

  def f = () => {
    @infunction
    def foo = 0
    ()
  }
}
