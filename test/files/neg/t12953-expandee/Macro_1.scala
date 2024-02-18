
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Macro {
  def id[A](body: A): A = macro impl[A]

  def impl[A: c.WeakTypeTag](c: Context)(body: c.Expr[A]) = {
    body
  }
}
