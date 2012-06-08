import scala.reflect.runtime.universe._

object Test extends App {
  def typeTagIsnotClassManifest[T: TypeTag] = {
    println(classManifest[T])
  }

  typeTagIsnotClassManifest[Int]
  typeTagIsnotClassManifest[String]
  typeTagIsnotClassManifest[Array[Int]]
}