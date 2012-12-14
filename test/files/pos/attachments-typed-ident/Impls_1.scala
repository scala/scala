import scala.reflect.macros.Context
import language.experimental.macros

object MyAttachment

object Macros {
  def impl(c: Context) = {
    import c.universe._
    val ident = Ident(newTermName("bar")) updateAttachment MyAttachment
    assert(ident.attachments.get[MyAttachment.type].isDefined, ident.attachments)
    val typed = c.typeCheck(ident)
    assert(typed.attachments.get[MyAttachment.type].isDefined, typed.attachments)
    c.Expr[Int](typed)
  }

  def foo = macro impl
}