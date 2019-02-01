import scala.reflect.runtime.universe._
import scala.reflect.ClassManifest

object Test extends App {
  def weakTypeTagIsnotClassManifest[T: WeakTypeTag] = {
    println(implicitly[ClassManifest[T]])
  }

  weakTypeTagIsnotClassManifest[Int]
  weakTypeTagIsnotClassManifest[String]
  weakTypeTagIsnotClassManifest[Array[Int]]
}
