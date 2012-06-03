import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo(c: Ctx)(x: c.Expr[Int], y: c.Expr[Int]) = {
    import c.universe._
    val sum = Apply(Select(x.tree, newTermName("$minus")), List(y.tree))
    val body = Apply(Select(Ident(definitions.PredefModule), newTermName("println")), List(sum))
    Expr[Unit](body)
  }
}