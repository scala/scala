class Test {
  case class Foo(x: int, private var y: int)
}

object Test {
  val test = new Test
  val x = test.Foo(1, 2)
  x match {
    case test.Foo(x, y) => println(x); println(y)
  }
}
