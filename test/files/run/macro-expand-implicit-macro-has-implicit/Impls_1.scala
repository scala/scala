import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo(c: Ctx)(x: c.Expr[Int]) = {
    import c.mirror._
    val body = Apply(Select(Ident(definitions.PredefModule), newTermName("println")), List(x.tree))
    Expr[Unit](body)
  }
}
