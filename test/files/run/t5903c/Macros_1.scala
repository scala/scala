import scala.reflect.macros.WhiteboxContext
import language.experimental.macros

object Interpolation {
  implicit class TestInterpolation(c: StringContext) {
    object t {
      def unapply[T](x: T): Any = macro Macros.unapplyImpl[T]
    }
  }
}

object Macros {
  def unapplyImpl[T: c.WeakTypeTag](c: WhiteboxContext)(x: c.Tree) = {
    import c.universe._
    q"""
      new {
        def isEmpty = false
        def get = 2
        def unapply(x: Int) = this
      }.unapply($x)
    """
  }
}
