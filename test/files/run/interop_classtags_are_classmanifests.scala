import scala.reflect.{ClassTag, classTag}

object Test extends App {
  def classTagIsClassManifest[T: ClassTag] = {
    println(classManifest[T])
  }

  classTagIsClassManifest[Int]
  classTagIsClassManifest[String]
  classTagIsClassManifest[Array[Int]]
}