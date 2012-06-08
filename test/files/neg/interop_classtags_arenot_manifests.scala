import scala.reflect.{ClassTag, classTag}

object Test extends App {
  def classTagIsnotManifest[T: ClassTag] = {
    println(manifest[T])
  }

  classTagIsnotManifest[Int]
  classTagIsnotManifest[String]
  classTagIsnotManifest[Array[Int]]
}