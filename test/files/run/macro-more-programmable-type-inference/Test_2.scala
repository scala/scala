import scala.reflect.runtime.universe._

object Test extends App {
  def bar[T: TypeTag](foo: Foo[T]) = println(typeOf[T])
  bar(0)
  bar(1)
}