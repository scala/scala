import scala.reflect.macros.BlackboxContext

object Macros {
  def returnsString(i: Double): String = macro returnsIntImpl
  def returnsIntImpl(c: BlackboxContext)(i: c.Expr[Double]): c.Expr[Int] = ???
}
