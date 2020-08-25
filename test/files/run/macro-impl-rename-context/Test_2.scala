// scalac: -language:experimental.macros
object Test extends App {
  println("foo")
  Macros.foo(42)
}
