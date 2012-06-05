import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo(c: Ctx)(xs: c.Expr[Int]*) = {
    import c.universe._
    val stripped_xs = xs map (_.tree) toList match {
      case List(Typed(stripped, Ident(wildstar))) if wildstar == tpnme.WILDCARD_STAR => List(stripped)
      case _ => ???
    }
    val body = Apply(Select(Ident(definitions.PredefModule), newTermName("println")), stripped_xs)
    c.Expr[Unit](body)
  }
}