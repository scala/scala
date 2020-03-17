
@deprecated("Suppress warnings", since="2.11")
object Test extends App {
  import scala.reflect.ClassManifest
  println(implicitly[ClassManifest[Int]])
  println(implicitly[ClassManifest[Int]] eq Manifest.Int)
}
