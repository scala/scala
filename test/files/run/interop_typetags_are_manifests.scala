import scala.reflect.runtime.universe._
import scala.reflect.ClassTag

object Test extends App {
  def typeTagIsManifest[T: TypeTag : ClassTag] = {
    println(manifest[T])
  }

  typeTagIsManifest[Int]
  typeTagIsManifest[String]
  typeTagIsManifest[Array[Int]]
}