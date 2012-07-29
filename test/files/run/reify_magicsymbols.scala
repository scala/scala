import scala.reflect.runtime.universe._

object Test extends App {
  println(typeOf[Any])
  println(typeOf[AnyVal])
  println(typeOf[AnyRef])
  println(typeOf[Null])
  println(typeOf[Nothing])
  println(typeOf[List[Any]])
  println(typeOf[List[AnyVal]])
  println(typeOf[List[AnyRef]])
  println(typeOf[List[Null]])
  println(typeOf[List[Nothing]])
  println(typeOf[{def foo(x: Int): Int}])
  println(typeOf[(Int*) => Unit])
  println(typeOf[(=> Int) => Unit])
}