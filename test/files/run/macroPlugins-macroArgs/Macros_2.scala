import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Macros {
  def impl(c: Context)(arg: c.Tree) = {
    import c.universe._
    q"""println($arg)"""
  }

  def foo(arg: String): Unit = macro impl
}