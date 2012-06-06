package scala.reflect.makro
package runtime

trait Exprs {
  self: Context =>

  def Expr[T: TypeTag](tree: Tree): Expr[T] = mirror.Expr[T](mirror.rootMirror, mirror.FixedMirrorTreeCreator(mirror.rootMirror, tree))
}
