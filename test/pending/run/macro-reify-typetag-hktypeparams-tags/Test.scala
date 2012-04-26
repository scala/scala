import scala.reflect.mirror._

object Test extends App {
  def fooTypeTagHK[C[_]: TypeTag, T: TypeTag] = {
    println(implicitly[TypeTag[C[T]]])
    println(implicitly[TypeTag[List[C[T]]]])
  }
  fooTypeTagHK[List, Int]
}