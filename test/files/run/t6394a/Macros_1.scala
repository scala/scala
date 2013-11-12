import scala.reflect.macros.BlackboxContext

object Macros {
  def impl(c:BlackboxContext): c.Expr[Any] = {
    import c.universe._

    val selfTree = This(c.enclosingImpl.symbol.asModule.moduleClass)
    c.Expr[AnyRef](selfTree)
  }

  def foo: Any = macro impl
}