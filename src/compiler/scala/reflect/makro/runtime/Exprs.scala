package scala.reflect.makro
package runtime

trait Exprs {
  self: Context =>

  def Expr[T: AbsTypeTag](tree: Tree): Expr[T] = universe.Expr[T](mirror, universe.FixedMirrorTreeCreator(mirror, tree))
}
