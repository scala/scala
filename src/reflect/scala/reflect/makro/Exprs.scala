package scala.reflect
package makro

trait Exprs {
  self: Context =>

  def Expr[T: AbsTypeTag](tree: Tree): Expr[T]
}
