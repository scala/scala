
@deprecated("Suppress warnings", since="2.11")
object Test extends App {
  type CM[T] = ClassManifest[T]
  println(implicitly[CM[Int]])
  println(implicitly[CM[Int]] eq Manifest.Int)
}
