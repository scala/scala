import scala.reflect.runtime.universe._

object Test extends App {
  def classManifestIsnotConcreteTypeTag[T: ClassManifest] = {
    println(implicitly[ConcreteTypeTag[T]])
  }

  classManifestIsnotConcreteTypeTag[Int]
  classManifestIsnotConcreteTypeTag[String]
  classManifestIsnotConcreteTypeTag[Array[Int]]
}