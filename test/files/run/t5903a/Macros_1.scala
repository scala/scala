import scala.reflect.macros.whitebox.Context
import language.experimental.macros

trait Tree
case object SomeTree extends Tree

object NewQuasiquotes {
  implicit class QuasiquoteInterpolation(c: StringContext) {
    object nq {
      def unapply(t: Tree): Any = macro QuasiquoteMacros.unapplyImpl
    }
  }
}

object QuasiquoteMacros {
  def unapplyImpl(c: Context)(t: c.Tree) = {
    import c.universe._
    q"""
      new {
        def isEmpty = false
        def get = this
        def _1 = SomeTree
        def _2 = SomeTree
        def unapply(t: Tree) = this
      }.unapply($t)
    """
  }
}
