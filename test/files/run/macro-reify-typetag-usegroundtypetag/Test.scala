import scala.reflect.mirror._

object Test extends App {
  def fooTypeTag[T: GroundTypeTag] = {
    println(implicitly[TypeTag[T]])
    println(implicitly[TypeTag[List[T]]])
  }
  fooTypeTag[Int]
}