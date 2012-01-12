object Test extends App {
  case class Foo(x: Int, y: String)

  Foo(2, "3") match {
    case Foo(x, y) => println((x, y))
  }

  case class FooSeq(x: Int, y: String, z: Boolean*)

  FooSeq(2, "3") match {
    case FooSeq(x, y) => println((x, y))
  }

  FooSeq(2, "3", true, false, true) match {
    case FooSeq(x, y) => println("nope")
    case FooSeq(x, y, true, false, true) => println((x, y))
  }

  FooSeq(1, "a", true, false, true) match {
    case FooSeq(1, "a") => println("nope")
    case FooSeq(1, "a", x@_* ) => println(x.toList)
  }
}