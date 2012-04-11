object Test extends App {
  val list: List[List[String]] = Macros.foo(List("hello world"))
  println(list)
}