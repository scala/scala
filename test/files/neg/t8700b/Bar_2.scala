object Bar {
  def bar1(foo: Foo_1) = foo match {
    case Foo_1.A => 1
  }

  def bar2(foo: Baz_1) = foo match {
    case Baz_1.A => 1
    }
}
