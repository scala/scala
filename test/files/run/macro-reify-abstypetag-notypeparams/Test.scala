import scala.reflect.runtime.universe._

object Test extends App {
  println(implicitly[WeakTypeTag[Int]])
  println(implicitly[WeakTypeTag[List[Int]]])
}