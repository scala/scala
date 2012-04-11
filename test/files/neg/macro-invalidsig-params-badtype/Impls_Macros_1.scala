import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo(c: Ctx)(x: c.mirror.Tree) = ???
}

object Macros {
  def foo(x: Int) = macro Impls.foo
}
