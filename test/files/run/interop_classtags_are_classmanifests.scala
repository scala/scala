import scala.reflect.{ClassManifest, ClassTag}

@deprecated("Suppress warnings", since="2.11")
object Test extends App {
  def classTagIsClassManifest[T: ClassTag] = {
    println(implicitly[ClassManifest[T]])
  }

  classTagIsClassManifest[Int]
  classTagIsClassManifest[String]
  classTagIsClassManifest[Array[Int]]
}
