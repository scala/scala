package scala.reflect
package macros

trait Evals {
  self: Context =>

  /** .. */
  def eval[T](expr: Expr[T]): T
}