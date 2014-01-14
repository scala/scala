import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.reflect.macros.ParseException

object Macros {
  def impl(c: Context)() = {
    import c.universe._
    val out = try {
      c.parse("foo(bar")
      "didn't fail"
    } catch {
      case e: ParseException =>
        s"failed with '${e.pos}' position and '${e.msg}' message"
    }
    c.Expr[String](Literal(Constant(out)))
  }
  def foo(): String = macro impl
}