trait B
class D extends B
object Test extends App {
  def foo[T](bar: T) = {
    bar match {
      case _: Array[Array[_]] => println("array 2d")
      case _: Array[_] => println("array 1d")
      case _ => println("something else")
    }
  }
  foo(Array.fill(10)(2))
  foo(Array.fill(10, 10)(2))
  foo(Array.fill(10, 10, 10)(2))
  foo(List(1, 2, 3))
}
