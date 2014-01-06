import scala.reflect.macros.WhiteboxContext
import language.experimental.macros

object Extractor {
  def unapply(x: Any): Any = macro unapplyImpl
  def unapplyImpl(c: WhiteboxContext)(x: c.Tree) = {
    val st = c.universe.asInstanceOf[reflect.internal.SymbolTable]
    import st._
    val subpatterns = x.attachments.get[SubpatternsAttachment].get.patterns
    q"""
      new {
        def isEmpty = false
        def get = ${subpatterns.toString}
        def unapply(x: Any) = this
      }.unapply(${x.asInstanceOf[st.Tree]})
    """.asInstanceOf[c.Tree]
  }
}
