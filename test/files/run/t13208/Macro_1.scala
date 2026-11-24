import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object M {
  def m(x: Long): Any = macro impl
  def impl(c: Context)(x: c.Tree): c.Tree = c.untypecheck(x)
}
