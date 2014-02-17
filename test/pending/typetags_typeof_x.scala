import scala.reflect.runtime.universe._

object Test extends App {
  def foo[T](x: T) = weakTypeOf(List(x))
  println(foo(2))
  locally { class C; println(weakTypeOf(new C)) }

  println(typeOf(2))
  println(typeOf(List(1, "1")))
  println(typeOf(new { def x = 2 }))
  println(typeOf[Null])
  println(typeOf[Nothing])
  println(typeOf(null))
}