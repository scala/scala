import scala.language.experimental.macros
import scala.reflect.macros.{Context => Ctx, ParseException}

object Macros {
  def impl(c: Ctx)() = {
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