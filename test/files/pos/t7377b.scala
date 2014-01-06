object Test {
  List(1) match { case Nil => 0; case (x::xs) => x }

  case class Foo(a: Int)
  val FooAlias: Foo.type = Foo
  Foo(0) match { case FooAlias(_) => 0 }
  Foo(0) match { case Foo(_) => 0 }

  case class Bar()
  val BarAlias: Bar.type = Bar
  Bar() match { case BarAlias() => 0 }
  Bar() match { case Bar() => 0 }
}
