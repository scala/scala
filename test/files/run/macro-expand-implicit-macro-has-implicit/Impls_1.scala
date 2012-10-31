import scala.reflect.macros.{Context => Ctx}

object Impls {
  def foo(c: Ctx)(x: c.Expr[Int]) = {
    import c.universe._
    val body = Apply(Select(Ident(definitions.PredefObject), newTermName("println")), List(x.tree))
    c.Expr[Unit](body)
  }
}