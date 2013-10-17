import scala.reflect.macros.Context
import language.experimental.macros

object Macros {
  def impl(c: Context) = {
    import c.universe._
    val wut = c.typeCheck(Select(Literal(Constant(10)), newTermName("$minus")), silent = true)
    // println(showRaw(wut, printIds = true, printTypes = true))
    c.Expr[Unit](Literal(Constant(())))
  }

  def foo = macro impl
}