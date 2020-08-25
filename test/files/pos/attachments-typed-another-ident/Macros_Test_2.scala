// scalac: -language:experimental.macros
object Test extends App {
  def bar = 2
  Macros.foo
}

