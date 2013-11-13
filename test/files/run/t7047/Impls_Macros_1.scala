import scala.reflect.macros.BlackboxContext
import language.experimental.macros

class Foo

object Macros {
  def impl(c: BlackboxContext) = {
    import c.universe._
    try {
      c.inferImplicitValue(typeOf[Foo], silent = false)
      c.abort(c.enclosingPosition, "silent=false is not working")
    } catch {
      case _: Exception =>
    }
    c.Expr[Null](Literal(Constant(null)))
  }

  def foo = macro impl
}