object Test extends App {
  val arr = Macros.foo("hello", "world")
  println(arr.getClass)
}