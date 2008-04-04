object Test1 {
  implicit def cast[A, B](x: A)(implicit c: A => B): B = c(x)

  val x1: String = 1
  val x2: String = cast[Int, String](1)
}
object Test2 {
  class Foo
  class Bar
  class Baz
  implicit def foo2bar(x: Foo)(implicit baz2bar: Baz => Bar): Bar = baz2bar(new Baz)
  implicit def baz2bar(x: Baz)(implicit foo2bar: Foo => Bar): Bar = foo2bar(new Foo)

  val x: Bar = new Foo
  val y: Bar = new Baz
}
