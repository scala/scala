import scala.reflect.macros.blackbox.Context
import language.experimental.macros

object MyAttachment

object Macros {
  def impl(c: Context) = {
    import c.universe._
    import internal._
    val ident = updateAttachment(Ident(TermName("bar")), MyAttachment)
    assert(attachments(ident).get[MyAttachment.type].isDefined, attachments(ident))
    val typed = c.typecheck(ident)
    assert(attachments(typed).get[MyAttachment.type].isDefined, attachments(typed))
    c.Expr[Int](typed)
  }

  def foo = macro impl
}