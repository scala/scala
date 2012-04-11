import scala.reflect.mirror._

object Test extends App {
  def fooTypeTagHK[C[_]: GroundTypeTag, T: GroundTypeTag] = {
    println(implicitly[GroundTypeTag[C[T]]])
    println(implicitly[GroundTypeTag[List[C[T]]]])
  }
  fooTypeTagHK[List, Int]
}