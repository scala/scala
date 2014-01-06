class Foo(val x: Int) extends AnyVal

@deprecated("Suppress warnings", since="2.11")
object Test extends App {
  println(classManifest[Foo])
}
