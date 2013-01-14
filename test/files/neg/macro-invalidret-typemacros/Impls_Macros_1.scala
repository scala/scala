import scala.reflect.macros.{Context => Ctx}

object Impls_Macros {
  def foo(c: Ctx) = c.universe.reify(2)
  type Foo = macro foo
}