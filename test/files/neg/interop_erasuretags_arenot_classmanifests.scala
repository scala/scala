object Test extends App {
  def erasureTagIsnotClassManifest[T: ErasureTag] = {
    println(classManifest[T])
  }

  erasureTagIsnotClassManifest[Int]
  erasureTagIsnotClassManifest[String]
  erasureTagIsnotClassManifest[Array[Int]]
}