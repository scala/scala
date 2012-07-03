object Test extends App {
  println(classManifest[Int])
  println(classManifest[Int] eq Manifest.Int)
}