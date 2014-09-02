import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

trait Base
class Att extends Base

object Macros {
  def impl(c: Context) = {
    import c.universe._
    import c.internal._
    import decorators._
    val dummy = q"x"
    dummy.updateAttachment(new Att)
    if (dummy.attachments.get[Base].isEmpty) c.abort(c.enclosingPosition, "that's not good")
    q"()"
  }

  def foo: Any = macro impl
}