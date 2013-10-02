import scala.reflect.macros.BlackboxContext
import language.experimental.macros

object Interpolation {
  implicit class TestInterpolation(c: StringContext) {
    object t {
      def unapply(x: Int) = macro Macros.unapplyImpl
    }
  }
}

object Macros {
  def unapplyImpl(c: BlackboxContext)(x: c.Tree) = {
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
