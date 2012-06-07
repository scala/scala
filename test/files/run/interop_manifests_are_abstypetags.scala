import scala.reflect.runtime.universe._

object Test extends App {
  def manifestIsAbsTypeTag[T: Manifest] = {
    println(implicitly[AbsTypeTag[T]].tpe)
  }

  manifestIsAbsTypeTag[Int]
  manifestIsAbsTypeTag[String]
  manifestIsAbsTypeTag[Array[Int]]
}