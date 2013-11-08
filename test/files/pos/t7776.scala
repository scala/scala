import scala.language.experimental.macros
import scala.reflect.macros.Context

class MacroErasure {
  def app(f: Any => Any, x: Any): Any = macro MacroErasure.appMacro
  def app[A](f: A => Any, x: Any): Any = macro MacroErasure.appMacroA[A]
}

object MacroErasure {
  def appMacro(c: Context)(f: c.Expr[Any => Any], x: c.Expr[Any]): c.Expr[Any] = ???
  def appMacroA[A](c: Context)(f: c.Expr[A => Any], x: c.Expr[Any])(implicit tt: c.WeakTypeTag[A]): c.Expr[Any] = ???
}