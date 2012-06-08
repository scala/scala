import scala.reflect.runtime.universe._

object Test extends App {
  def absTypeTagIsnotClassManifest[T: AbsTypeTag] = {
    println(classManifest[T])
  }

  absTypeTagIsnotClassManifest[Int]
  absTypeTagIsnotClassManifest[String]
  absTypeTagIsnotClassManifest[Array[Int]]
}