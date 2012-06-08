import scala.reflect.runtime.universe._

object Test extends App {
  def absTypeTagIsnotManifest[T: AbsTypeTag] = {
    println(manifest[T])
  }

  absTypeTagIsnotManifest[Int]
  absTypeTagIsnotManifest[String]
  absTypeTagIsnotManifest[Array[Int]]
}