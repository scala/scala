object Macros {
  def foo = macro Impls.foo
}

object Test extends App {
  val firstClassFoo = Macros.foo _
  firstClassFoo
}