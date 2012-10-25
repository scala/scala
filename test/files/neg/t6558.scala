class AnnotNotFound {
  def foo(a: Any) = ()

  foo {
    @sth
    def foo = 0
    foo
  }
}
