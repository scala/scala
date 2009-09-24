object Test {
  class Foo
  val foo = new Foo
  val x = foo.blah match {
    case List(x) => x
    case Nil => null
    case _ => throw new Error("too many!")
  }
}
