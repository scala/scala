
@deprecated("Suppress warnings", since="2.11")
object Test {
  def main(args: Array[String]): Unit = {
    assert(classManifest[Array[String]].typeArguments contains classManifest[String])
    assert(classManifest[Array[Int]].typeArguments contains classManifest[Int])
    assert(classManifest[Array[Float]].typeArguments contains classManifest[Float])
    assert(manifest[Array[String]].typeArguments contains manifest[String])
    assert(manifest[Array[Int]].typeArguments contains manifest[Int])
    assert(manifest[Array[Float]].typeArguments contains manifest[Float])
  }
}
