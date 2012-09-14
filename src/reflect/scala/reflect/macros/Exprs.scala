package scala.reflect
package macros

trait Exprs {
  self: Context =>

  def Expr[T: WeakTypeTag](tree: Tree): Expr[T]
}
