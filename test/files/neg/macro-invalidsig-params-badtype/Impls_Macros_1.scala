import scala.reflect.macros.{BlackboxContext => Ctx}

object Impls {
  def foo(c: Ctx)(x: Int) = ???
}

object Macros {
  def foo(x: Int) = macro Impls.foo
}
