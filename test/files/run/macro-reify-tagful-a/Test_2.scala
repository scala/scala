object Test extends App {
  val list: List[String] = Macros.foo("hello world")
  println(list)
}