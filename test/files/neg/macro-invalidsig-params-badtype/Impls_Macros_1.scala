import scala.reflect.macros.{Context => Ctx}

object Impls {
  def foo(c: Ctx)(x: c.universe.Tree) = ???
}

object Macros {
  def foo(x: Int) = macro Impls.foo
}
