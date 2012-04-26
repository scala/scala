import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo(c: Ctx)(xs: c.Expr[Int]*) = {
    import c.mirror._
    val body = Apply(Select(Ident(definitions.PredefModule), newTermName("println")), xs.map(_.tree).toList)
    Expr[Unit](body)
  }
}
