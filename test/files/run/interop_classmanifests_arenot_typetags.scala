import scala.reflect.runtime.universe._

object Test extends App {
  def classManifestIsnotTypeTag[T: ClassManifest] = {
    println(typeOf[T])
  }

  classManifestIsnotTypeTag[Int]
  classManifestIsnotTypeTag[String]
  classManifestIsnotTypeTag[Array[Int]]
}