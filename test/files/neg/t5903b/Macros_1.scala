import scala.reflect.macros.blackbox.Context
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
        def get = "2"
        def unapply(x: String) = this
      }.unapply($x)
    """
  }
}
