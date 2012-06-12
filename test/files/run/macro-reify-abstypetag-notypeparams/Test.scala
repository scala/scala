import scala.reflect.runtime.universe._

object Test extends App {
  println(implicitly[AbsTypeTag[Int]])
  println(implicitly[AbsTypeTag[List[Int]]])
}