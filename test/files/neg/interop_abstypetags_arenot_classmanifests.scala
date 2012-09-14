import scala.reflect.runtime.universe._

object Test extends App {
  def weakTypeTagIsnotClassManifest[T: WeakTypeTag] = {
    println(classManifest[T])
  }

  weakTypeTagIsnotClassManifest[Int]
  weakTypeTagIsnotClassManifest[String]
  weakTypeTagIsnotClassManifest[Array[Int]]
}