import scala.reflect.runtime.universe._

object Test extends App {
  def manifestIsConcreteTypeTag[T: Manifest] = {
    println(implicitly[ConcreteTypeTag[T]].tpe)
  }

  manifestIsConcreteTypeTag[Int]
  manifestIsConcreteTypeTag[String]
  manifestIsConcreteTypeTag[Array[Int]]
}