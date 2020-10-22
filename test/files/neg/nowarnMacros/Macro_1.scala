import scala.reflect.macros.blackbox.Context

object Macro {
  def discard(c: Context)(expr: c.Expr[Any]): c.Tree = {
    import c.universe._
    q"()"
  }
}
