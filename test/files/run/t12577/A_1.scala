import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object A {
  def foo[A, B](pf: PartialFunction[A, B]): PartialFunction[A, B] = macro impl
  def impl(c: Context)(pf: c.Tree): c.Tree = c.untypecheck(pf)
}
