import scala.reflect.ClassManifest

@deprecated("Suppress warnings", since="2.11")
object Test extends App {
  println(implicitly[ClassManifest[Int]])
  println(implicitly[ClassManifest[Int]] eq Manifest.Int)
}
