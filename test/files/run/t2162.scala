class Foo(x: Int)
class Bar extends Foo(1)

trait A {
  def foo[T <: Foo]: Unit
}
class B extends A {
  def foo[Bar]: Unit = { println("B.foo[Bar]") }
}
object Test {
  val x = new B
  val y = new A {
    def foo[Bar]: Unit = { println("A.foo[Bar]") }
  }
  def main(args: Array[String]): Unit = {
    x.foo // ok
    y.foo // java.lang.AssertionError: assertion failed (Erasure.scala:441 in r18338))
  }
}
