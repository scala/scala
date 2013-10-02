import scala.reflect.macros.{BlackboxContext => Ctx}

object Impls {
  def foo(c: Ctx)(x: c.Expr[Int]) = {
    import c.universe._
    val body = Apply(Select(Ident(definitions.PredefModule), TermName("println")), List(x.tree))
    c.Expr[Unit](body)
  }
}