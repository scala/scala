@deprecated("Suppress warnings", since="2.11")
object Test extends App {
  println(classManifest[Int])
  println(classManifest[Int] eq Manifest.Int)
}
