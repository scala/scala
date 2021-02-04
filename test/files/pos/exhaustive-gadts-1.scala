sealed trait Expr[+T]
case class IntExpr(x: Int) extends Expr[Int]
case class BooleanExpr(b: Boolean) extends Expr[Boolean]

object Test {
  def foo[T](x: Expr[T], y: Expr[T]) = (x, y) match {
    case (IntExpr(_), IntExpr(_)) =>
    case (BooleanExpr(_), BooleanExpr(_)) =>
  }
}
