class Foo[T](val x: T) extends AnyVal

@deprecated("Suppress warnings", since="2.11")
object Test extends App {
  println(classManifest[Foo[String]])
}
