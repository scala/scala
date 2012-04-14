import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo(c: Ctx)(x: c.Expr[Int]) = {
    import c.mirror._
    val body = Apply(Select(x.tree, newTermName("$plus")), List(Literal(Constant(1))))
    Expr[Int](body)
  }
}
