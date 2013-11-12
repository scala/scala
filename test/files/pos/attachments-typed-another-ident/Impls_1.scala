import scala.reflect.macros.BlackboxContext
import language.experimental.macros

object MyAttachment

object Macros {
  def impl(c: BlackboxContext) = {
    import c.universe._
    val ident = Ident(TermName("bar")) updateAttachment MyAttachment
    assert(ident.attachments.get[MyAttachment.type].isDefined, ident.attachments)
    val typed = c.typeCheck(ident)
    assert(typed.attachments.get[MyAttachment.type].isDefined, typed.attachments)
    c.Expr[Int](typed)
  }

  def foo = macro impl
}
