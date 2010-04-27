trait Tr

class Foo[@specialized(Int) T](_x: T) extends {
  val bar = "abc"
  val baz = "bbc"
} with Tr {
  val x = _x
  println(x)
  println(bar)
}

object Test extends Application {
  new Foo("a")
  new Foo(42)
}
