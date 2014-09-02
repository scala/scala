import scala.reflect.macros.whitebox.Context
import language.experimental.macros

object Extractor {
  def unapply(x: Any): Any = macro unapplyImpl
  def unapplyImpl(c: Context)(x: c.Tree) = {
    import c.universe._
    import internal._
    val pos = subpatterns(x).get.head.pos
    q"""
      new {
        def isEmpty = false
        def get = ${"The width of the subpattern is: " + (pos.end - pos.start + 1)}
        def unapply(x: Any) = this
      }.unapply($x)
    """
  }
}
