package scala.reflect
package macros

trait Exprs {
  self: Context =>

  def Expr[T: AbsTypeTag](tree: Tree): Expr[T]
}
