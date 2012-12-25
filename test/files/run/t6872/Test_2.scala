trait Foo

object Test extends Mock with App {
  val m = new mock[Foo](42)
  println("Hello world")
}