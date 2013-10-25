import scala.reflect.macros.Context

object Impls {
  def impl(c: Context) = {
    import c.universe._
    reify {
      println(c.Expr[String](Literal(Constant(c.settings.toString))).splice)
    }
  }
}

object Macros {
  def foo = macro Impls.impl
}