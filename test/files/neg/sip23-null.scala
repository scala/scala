object Test {
  def bar(x: Any): x.type = null
  def foo(x: AnyVal): x.type = null
  def baz(x: AnyRef): x.type = null // ok
}

