object Test extends App {
  val foo = mkObject("x" -> "2", "y" -> 3)
  val bar: { def x: Any; def y: Any } = foo
  println(bar.x)
  println(bar.y)
}