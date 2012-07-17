class Foo(val x: Int) extends AnyVal

object Test extends App {
  println(manifest[Foo])
}