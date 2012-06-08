import scala.reflect.runtime.universe._

object Test extends App {
  def foo[T: TypeTag] = println(implicitly[TypeTag[T]])
  foo
}