import scala.reflect.macros.Context
import scala.language.experimental.macros

class X

object Macros {
  def ctor(c: Context)(x: c.Tree): c.Expr[X] = {
    import c.universe._
    c.Expr[X](q"C($x)")
  }
}
