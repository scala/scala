import scala.reflect.{ClassTag, classTag}

@deprecated("Suppress warnings", since="2.11")
object Test extends App {
  def classManifestIsClassTag[T: ClassManifest] = {
    println(classTag[T])
    println(Array[T]().toList)
    println(new Array[T](5).toList)
  }

  classManifestIsClassTag[Int]
  classManifestIsClassTag[String]
  classManifestIsClassTag[Array[Int]]

  def manifestIsClassTag[T: Manifest] = {
    println(classTag[T])
    println(Array[T]().toList)
    println(new Array[T](5).toList)
  }

  manifestIsClassTag[Int]
  manifestIsClassTag[String]
  manifestIsClassTag[Array[Int]]
}
