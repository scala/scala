import scala.reflect.mirror._

object Test extends App {
  def fooNoTypeTagHK[C[_], T] = {
    println(implicitly[GroundTypeTag[C[T]]])
    println(implicitly[GroundTypeTag[List[C[T]]]])
  }
  fooNoTypeTagHK[List, Int]
}