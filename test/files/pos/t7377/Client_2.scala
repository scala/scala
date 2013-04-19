object Test {
  M.noop(List(1) match { case Nil => 0; case (x::xs) => x })

  case class Foo(a: Int)
  val FooAlias: Foo.type = Foo
  M.noop(Foo(0) match { case FooAlias(_) => 0 })

  case class Bar()
  val BarAlias: Bar.type = Bar
  M.noop(Bar() match { case BarAlias() => 0 })
}
