import language.experimental.macros

object Test extends App {
  val macros = new { def foo = macro Impls.foo }
  macros.foo
}