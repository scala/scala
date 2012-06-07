import scala.reflect.runtime.universe._
import scala.reflect.ClassTag

object Test extends App {
  def typeTagWithoutClassTagIsnotManifest[T: TypeTag] = {
    println(manifest[T])
  }

  typeTagWithoutClassTagIsnotManifest[Int]
  typeTagWithoutClassTagIsnotManifest[String]
  typeTagWithoutClassTagIsnotManifest[Array[Int]]
}