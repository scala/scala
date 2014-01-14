import scala.reflect.macros.whitebox.Context
import language.experimental.macros

object Interpolation {
  implicit class TestInterpolation(c: StringContext) {
    object t {
      def unapply(x: Int): Any = macro Macros.unapplyImpl
    }
  }
}

object Macros {
  def unapplyImpl(c: Context)(x: c.Tree) = {
    import c.universe._
    q"""
      new {
        class Match(x: Int) extends AnyVal {
          def isEmpty = false
          def get = x
        }
        def unapply(x: Int) = new Match(x)
      }.unapply($x)
    """
  }
}
