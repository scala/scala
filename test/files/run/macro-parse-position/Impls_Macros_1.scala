import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Macros {
  def impl(c: Context)() = {
    import c.universe._
    val t = c.parse("foo bar")
    val out = s"${t.pos == NoPosition}\n${t.pos}\n${t.pos.source.content.length}\n${new String(t.pos.source.content)}"
    c.Expr[String](Literal(Constant(out)))
  }
  def foo(): String = macro impl
}