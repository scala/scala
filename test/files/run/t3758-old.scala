import scala.reflect.ClassManifest

@deprecated("Suppress warnings", since="2.11")
object Test {
  def main(args: Array[String]): Unit = {
    assert(implicitly[ClassManifest[Array[String]]].typeArguments contains implicitly[ClassManifest[String]])
    assert(implicitly[ClassManifest[Array[Int]]].typeArguments contains implicitly[ClassManifest[Int]])
    assert(implicitly[ClassManifest[Array[Float]]].typeArguments contains implicitly[ClassManifest[Float]])
    assert(manifest[Array[String]].typeArguments contains manifest[String])
    assert(manifest[Array[Int]].typeArguments contains manifest[Int])
    assert(manifest[Array[Float]].typeArguments contains manifest[Float])
  }
}
