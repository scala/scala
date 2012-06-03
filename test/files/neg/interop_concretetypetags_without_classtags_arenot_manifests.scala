import scala.reflect.runtime.universe._
import scala.reflect.ClassTag

object Test extends App {
  def concreteTypeTagIsManifest[T: ConcreteTypeTag] = {
    println(manifest[T])
  }

  concreteTypeTagIsManifest[Int]
  concreteTypeTagIsManifest[String]
  concreteTypeTagIsManifest[Array[Int]]
}