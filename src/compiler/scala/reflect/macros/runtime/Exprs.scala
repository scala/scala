package scala.reflect.macros
package runtime

trait Exprs {
  self: Context =>

  def Expr[T: WeakTypeTag](tree: Tree): Expr[T] = universe.Expr[T](mirror, universe.FixedMirrorTreeCreator(mirror, tree))
}
