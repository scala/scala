import scala.reflect.macros.whitebox.Context
import language.experimental.macros

object Extractor {
  def unapply(x: Any): Any = macro unapplyImpl
  def unapplyImpl(c: Context)(x: c.Tree) = {
    import c.universe._
    import internal._
    val subpatterns = attachments(x).get[scala.reflect.internal.SymbolTable#SubpatternsAttachment].get.patterns.toString
    q"""
      new {
        def isEmpty = false
        def get = $subpatterns
        def unapply(x: Any) = this
      }.unapply($x)
    """
  }
}
