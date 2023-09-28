package example

import scala.language.experimental.macros
import scala.reflect.macros._

object Provider {
  def printTree(arg: Any): String = macro printTreeImpl
  def printTreeImpl(c: Context)(arg: c.Expr[Any]): c.Expr[String] = {
    val argStr = arg.tree.toString
    val literalStr = c.universe.Literal(c.universe.Constant(argStr))
    c.Expr[String](literalStr)
  }
}
