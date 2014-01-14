import scala.reflect.macros.blackbox.Context

object Macros {
  def returnsString(i: Double): String = macro returnsIntImpl
  def returnsIntImpl(c: Context)(i: c.Expr[Double]): c.Expr[Int] = ???
}
