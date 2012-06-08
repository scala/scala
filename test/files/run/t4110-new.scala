import scala.reflect.runtime.universe._

object Test extends App {
  def inferredType[T : TypeTag](v : T) = println(typeOf[T])

  trait A
  trait B

  inferredType(new A with B)

  val name = new A with B
  inferredType(name)
}