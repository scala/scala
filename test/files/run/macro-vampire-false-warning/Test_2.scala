object Test extends App {
  val foo = mkObject("x" -> "2", "y" -> 3)
  println(foo.x)
  println(foo.y)
  // println(foo.z) => will result in a compilation error
}