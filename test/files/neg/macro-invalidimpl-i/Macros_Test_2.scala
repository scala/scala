package foo

object Test extends App {
  def foo = macro Impls.impl
}
