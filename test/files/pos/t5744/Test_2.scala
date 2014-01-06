object Test extends App {
  import Macros._
  foo(42)
  implicit val s = ""
  bar(43)
}