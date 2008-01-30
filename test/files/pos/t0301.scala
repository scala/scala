package fos

abstract class Expr
case class Var extends Expr

object Analyzer {
  def substitution(expr: Expr, cls: (Var,Var)): Expr =
    expr match {
      case cls._2 => cls._1  // source of the error
      case _ => expr
    }
}
