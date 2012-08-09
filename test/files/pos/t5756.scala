import scala.reflect.runtime.universe._

object Test extends App {
  def tagme[T: TypeTag](x: T) = typeTag[T]
  val foo = tagme{object Bar; Bar}
}