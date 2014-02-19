import scala.reflect.macros.whitebox.Context
import language.experimental.macros

object Extractor {
  def unapply(x: Any): Any = macro unapplyImpl
  def unapplyImpl(c: Context)(x: c.Tree) = {
    import c.universe._
    import internal._
    q"""
      new {
        def isEmpty = false
        def get = ${subpatterns(x).toString}
        def unapply(x: Any) = this
      }.unapply($x)
    """
  }
}
