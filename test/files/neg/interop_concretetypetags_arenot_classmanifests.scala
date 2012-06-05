import scala.reflect.runtime.universe._

object Test extends App {
  def concreteTypeTagIsnotClassManifest[T: ConcreteTypeTag] = {
    println(classManifest[T])
  }

  concreteTypeTagIsnotClassManifest[Int]
  concreteTypeTagIsnotClassManifest[String]
  concreteTypeTagIsnotClassManifest[Array[Int]]
}