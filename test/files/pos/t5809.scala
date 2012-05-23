package scala.reflect

package object api {
  implicit class PimpedExpr[T](expr: Universe # Expr[T]) {
    def runtimeEval: T = {
      println("hello, dear")
      expr.eval
    }
  }
}