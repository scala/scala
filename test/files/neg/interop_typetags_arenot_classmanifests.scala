import scala.reflect.runtime.universe._
import scala.reflect.ClassManifest

object Test extends App {
  def typeTagIsnotClassManifest[T: TypeTag] = {
    println(implicitly[ClassManifest[T]])
  }

  typeTagIsnotClassManifest[Int]
  typeTagIsnotClassManifest[String]
  typeTagIsnotClassManifest[Array[Int]]
}
