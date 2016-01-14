object Bar {
  def bar1(foo: Foo) = foo match {
    case Foo.A => 1
  }

  def bar2(foo: Baz) = foo match {
    case Baz.A => 1
    }
}
