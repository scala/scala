
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Macro {
  def id[A](body: A): A = macro impl[A]

  def impl[A: c.WeakTypeTag](c: Context)(body: c.Expr[A]) = {
    import c.universe._
    q"""val unusedVariable = "42".toInt; $body"""
  }
}
