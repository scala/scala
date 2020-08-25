// scalac: -language:experimental.macros -Xmacro-settings:hello=1
object Test extends App {
  Macros.foo
}
