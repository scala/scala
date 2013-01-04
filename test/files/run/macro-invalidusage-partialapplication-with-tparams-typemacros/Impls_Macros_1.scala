import scala.reflect.macros.{Context => Ctx}

class C
object Impls {
  def foo(c: Ctx) = c.universe.Ident(c.universe.TypeName("C"))
}

object Macros {
  type Foo[T] = macro Impls.foo
}