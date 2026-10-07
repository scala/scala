//> using options -Werror -deprecation

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

class qqq(count: Int) {
  def macroTransform(annottees: Any*): Any = macro qqq.qqqImpl
}

object qqq {
  def qqqImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = ???
}
