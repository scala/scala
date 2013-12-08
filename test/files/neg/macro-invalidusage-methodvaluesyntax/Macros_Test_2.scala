object Macros {
  def foo: Unit = macro Impls.foo
}

object Test extends App {
  val firstClassFoo = Macros.foo _
  firstClassFoo
}