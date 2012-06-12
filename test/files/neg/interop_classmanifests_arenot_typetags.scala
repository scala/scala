import scala.reflect.runtime.universe._

object Test extends App {
  def classManifestIsnotTypeTag[T: ClassManifest] = {
    println(implicitly[TypeTag[T]])
  }

  classManifestIsnotTypeTag[Int]
  classManifestIsnotTypeTag[String]
  classManifestIsnotTypeTag[Array[Int]]
}