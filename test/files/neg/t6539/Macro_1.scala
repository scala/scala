import language.experimental.macros
import reflect.macros.Context

object M {
  def m(a: Any, b: Any): Any = macro mImpl
  def mImpl(c: Context)(a: c.Expr[Any], b: c.Expr[Any]) = a

  @reflect.internal.annotations.compileTimeOnly("cto may only be used as an argument to " + "m")
  def cto = 0
}
