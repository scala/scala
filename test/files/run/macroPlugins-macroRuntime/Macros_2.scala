import scala.language.experimental.macros
import scala.reflect.macros.BlackboxContext

object Macros {
  def impl(c: BlackboxContext)(arg: c.Tree) = {
    import c.universe._
    q"""println($arg)"""
  }

  def foo(arg: String): Unit = macro impl
}