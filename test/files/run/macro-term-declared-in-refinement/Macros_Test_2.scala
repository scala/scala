class Base

object Test extends App {
  val macros = new Base { def foo = macro Impls.foo }
  macros.foo
}