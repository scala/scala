import scala.reflect.macros.{Context => Ctx}

class C(x: Int)
object Macros {
  def foo(c: Ctx)(x: c.Expr[Int]) = {
    import c.universe._
    Apply(Ident(TypeName("C")), List(x.tree))
  }
  type Foo(x: Int) = macro foo
}
