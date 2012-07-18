class Foo[T](val x: T) extends AnyVal

object Test extends App {
  println(classManifest[Foo[String]])
}