import scala.reflect.macros.Context

object Macros {
  def impl(c:Context): c.Expr[Any] = {
    import c.universe._

    val selfTree = This(c.enclosingImpl.symbol.asObject.objectClass)
    c.Expr[AnyRef](selfTree)
  }

  def foo: Any = macro impl
}