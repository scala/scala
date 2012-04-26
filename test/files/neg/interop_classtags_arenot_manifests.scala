object Test extends App {
  def arrayTagIsnotManifest[T: ArrayTag] = {
    println(manifest[T])
  }

  arrayTagIsnotManifest[Int]
  arrayTagIsnotManifest[String]
  arrayTagIsnotManifest[Array[Int]]

  def classTagIsnotManifest[T: ClassTag] = {
    println(manifest[T])
  }

  classTagIsnotManifest[Int]
  classTagIsnotManifest[String]
  classTagIsnotManifest[Array[Int]]
}