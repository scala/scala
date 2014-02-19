
package t8013

// perverse macro to confuse Xlint

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Perverse {

  implicit class Impervolator(sc: StringContext) {
    def p(args: Any*): String = macro pImpl
  }

  // turn a nice interpolation into something that looks
  // nothing like an interpolation or anything we might
  // recognize, but which includes a "$id" in an apply.
  def pImpl(c: Context)(args: c.Expr[Any]*): c.Expr[String] = {
    import c.universe._
    val macroPos = c.macroApplication.pos
    val text = macroPos.source.lineToString(macroPos.line - 1) substring macroPos.column
    val tt = Literal(Constant(text))
    val tree = q"t8013.Perverse.pervert($tt)"
    c.Expr[String](tree)
  }

  // identity doesn't seem very perverse in this context
  //def pervert(text: String): String = text
  def pervert(text: String): String = {
    Console println s"Perverting [$text]"
    text
  }
}
