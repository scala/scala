// scalac: -language:experimental.macros
object Test extends App {
  {
    def foo: Unit = macro Impls.foo
    foo
  }
}
