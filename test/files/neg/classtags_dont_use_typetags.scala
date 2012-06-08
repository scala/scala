import scala.reflect.runtime.universe._

object Test extends App {
  def foo[T: TypeTag] = Array[T]()
}