object Test extends App {
  def typeTagIsnotManifest[T: TypeTag] = {
    println(manifest[T])
  }

  typeTagIsnotManifest[Int]
  typeTagIsnotManifest[String]
  typeTagIsnotManifest[Array[Int]]
}