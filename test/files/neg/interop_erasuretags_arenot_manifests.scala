object Test extends App {
  def erasureTagIsnotManifest[T: ErasureTag] = {
    println(manifest[T])
  }

  erasureTagIsnotManifest[Int]
  erasureTagIsnotManifest[String]
  erasureTagIsnotManifest[Array[Int]]
}