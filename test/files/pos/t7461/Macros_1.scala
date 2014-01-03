import scala.reflect.macros.BlackboxContext
import language.experimental.macros

object Macros {
  def impl(c: BlackboxContext) = {
    import c.universe._
    val wut = c.typecheck(Select(Literal(Constant(10)), newTermName("$minus")), silent = true)
    // println(showRaw(wut, printIds = true, printTypes = true))
    c.Expr[Unit](q"()")
  }

  def foo = macro impl
}