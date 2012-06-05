package scala.reflect
package makro

trait Exprs {
  self: Context =>

  def Expr[T: TypeTag](tree: Tree): Expr[T]
}
