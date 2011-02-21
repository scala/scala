trait Tr

class Foo[@specialized(Int) T](_x: T) extends {
  val bar = "abc"
  val baz = "bbc"
} with Tr {
  val x = _x
  println(x)
  println(bar)
}

object Test extends App {
  new Foo("a")
  new Foo(42)
  println(runtime.BoxesRunTime.integerBoxCount)
}
