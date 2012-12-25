import scala.reflect.macros.{Context => Ctx}

class C
object Impls {
  def foo(c: Ctx)(x: c.Expr[Int]) = c.universe.Ident(c.universe.TypeName("C"))
}
