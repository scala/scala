trait Foo

object Test extends Mock with App {
  implicit val x = 42
  val m = new mock[Foo]
  println("Hello world")
}