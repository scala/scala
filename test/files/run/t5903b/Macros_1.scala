import scala.reflect.macros.whitebox.Context
import language.experimental.macros

object Interpolation {
  implicit class TestInterpolation(c: StringContext) {
    object t {
      def unapply[T](x: T): Any = macro Macros.unapplyImpl[T]
    }
  }
}

object Macros {
  def unapplyImpl[T: c.WeakTypeTag](c: Context)(x: c.Tree) = {
    import c.universe._
    q"""
      new {
        def isEmpty = false
        def get = this
        def _1 = 2
        def unapply(x: Int) = this
        override def toString = "oops"
      }.unapply($x)
    """
  }
}
