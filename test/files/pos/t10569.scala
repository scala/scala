object Test {
  object Foo
  Tuple1[Foo.type](Foo) match {
    case Tuple1(foo: Singleton) => foo
  }
}
