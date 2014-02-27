import scala.reflect.macros.whitebox._
import scala.language.experimental.macros

object Macros {
  def impl(c: Context)(x: c.Expr[Boolean]): c.Expr[Boolean] = x
  def foo(x: Boolean): Boolean = macro impl
}