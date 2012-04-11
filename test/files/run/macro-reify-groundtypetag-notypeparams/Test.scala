import scala.reflect.mirror._

object Test extends App {
  println(implicitly[GroundTypeTag[Int]])
  println(implicitly[GroundTypeTag[List[Int]]])
}