object Test extends App {
  def arrayTagIsClassManifest[T: ArrayTag] = {
    println(classManifest[T])
  }

  arrayTagIsClassManifest[Int]
  arrayTagIsClassManifest[String]
  arrayTagIsClassManifest[Array[Int]]

  def classTagIsClassManifest[T: ClassTag] = {
    println(classManifest[T])
  }

  classTagIsClassManifest[Int]
  classTagIsClassManifest[String]
  classTagIsClassManifest[Array[Int]]
}