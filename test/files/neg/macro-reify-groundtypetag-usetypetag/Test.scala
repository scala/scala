import scala.reflect.mirror._

object Test extends App {
  def fooTypeTag[T: TypeTag] = {
    println(implicitly[GroundTypeTag[T]])
    println(implicitly[GroundTypeTag[List[T]]])
  }
  fooTypeTag[Int]
}