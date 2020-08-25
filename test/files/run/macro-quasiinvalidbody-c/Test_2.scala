// scalac: -language:experimental.macros
object Test extends App {
  import Macros._
  println(foo(42))
}
