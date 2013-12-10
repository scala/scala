object Test extends App {
  val x: Option[Int] = Macros.foo
  println(x)
}