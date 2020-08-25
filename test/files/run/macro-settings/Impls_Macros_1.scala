// scalac: -language:experimental.macros -Xmacro-settings:hello=1
import scala.reflect.macros.blackbox.Context

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
