import language.experimental.macros
import scala.reflect.macros.blackbox.Context

object M {
  def m(a: Any, b: Any): Any = macro mImpl
  def mImpl(c: Context)(a: c.Expr[Any], b: c.Expr[Any]) = c.universe.reify(println(a.splice))

  @reflect.internal.annotations.compileTimeOnly("cto may only be used as an argument to " + "m")
  def cto = 0
}
