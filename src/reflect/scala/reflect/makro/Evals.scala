package scala.reflect
package makro

trait Evals {
  self: Context =>

  /** .. */
  def eval[T](expr: Expr[T]): T
}