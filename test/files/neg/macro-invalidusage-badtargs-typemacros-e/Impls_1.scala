import scala.reflect.macros.{Context => Ctx}

class C
object Impls {
  def foo(c: Ctx) = c.universe.Ident(c.universe.TypeName("C"))
  def bar(c: Ctx)(x: c.Expr[Int]) = foo(c)
}
