import scala.reflect.macros.BlackboxContext

object Impls {
  def impl(c: BlackboxContext) = {
    import c.universe._
    reify {
      println(c.Expr[String](Literal(Constant(c.settings.toString))).splice)
    }
  }
}

object Macros {
  def foo = macro Impls.impl
}