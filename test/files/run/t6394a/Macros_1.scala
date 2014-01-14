import scala.reflect.macros.blackbox.Context

object Macros {
  def impl(c:Context): c.Expr[Any] = {
    import c.universe._

    val selfTree = This(c.enclosingImpl.symbol.asModule.moduleClass)
    c.Expr[AnyRef](selfTree)
  }

  def foo: Any = macro impl
}