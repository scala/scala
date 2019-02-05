class Bar {
  def f (x : { def g }) {}
  f (new Baz { def g })
}
