class Test {
  def foo: Unit = bar(Array(): _*)
  def bar(values: AnyRef*): Unit = ()
}