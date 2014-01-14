import scala.reflect.macros.blackbox.Context
import language.experimental.macros

object Macros {
  def impl(c: Context) = {
    import c.universe._
    val wut = c.typecheck(Select(Literal(Constant(10)), newTermName("$minus")), silent = true)
    // println(showRaw(wut, printIds = true, printTypes = true))
    c.Expr[Unit](q"()")
  }

  def foo = macro impl
}