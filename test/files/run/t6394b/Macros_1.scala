import scala.reflect.macros.blackbox.Context

object Macros {
  def impl(c:Context): c.Expr[Any] = {
    import c.universe._

    val selfTree = This(typeNames.EMPTY)
    c.Expr[AnyRef](selfTree)
  }

  def foo: Any = macro impl
}