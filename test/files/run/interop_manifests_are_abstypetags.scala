import scala.reflect.runtime.universe._

object Test extends App {
  def manifestIsWeakTypeTag[T: Manifest] = {
    println(implicitly[WeakTypeTag[T]].tpe)
  }

  manifestIsWeakTypeTag[Int]
  manifestIsWeakTypeTag[String]
  manifestIsWeakTypeTag[Array[Int]]
}