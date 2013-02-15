import scala.reflect.macros.Context
import language.experimental.macros

trait C

object Macros {
  def tm(c: Context)(x: c.Expr[Int])(y: c.Expr[Int]) = c.universe.Ident(c.universe.TypeName("C"))
  type TM(x: Int)(y: Int) = macro tm
}