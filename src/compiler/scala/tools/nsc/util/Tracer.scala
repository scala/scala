/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package util

import java.io.PrintStream

class Tracer(enabled: () => Boolean) {
  def out: PrintStream = System.out
  def intoString(x: Any): String = "" + x
  def stringify(x: Any): String = x match {
    case null                   => "null"
    case x: TraversableOnce[_]  => x map stringify mkString ", "
    case x: Product             => stringify(x.productIterator)
    case x: AnyRef              => intoString(x)
  }

  private val LBRACE = "{"
  private val RBRACE = "}"
  private var indentLevel = 0
  private def ind(s: String) = (" " * (indentLevel * 2)) + s
  private def indented[T](body: => T): T = {
    indentLevel += 1
    try body
    finally indentLevel -= 1
  }
  private def p(s: String) = {
    out.print(s)
    out.flush()
  }
  private def pin[T](x: T): T = {
    p(ind("" + x))
    x
  }
  def apply[T](name: String, args: => Any)(body: => T): T = {
    val result = body
    if (enabled()) {
      // concise output optimization
      val boolResult = result match {
        case x: Boolean => Some(x)
        case _          => None
      }
      p(ind("%s(%s) = %s\n".format(
        name,
        stringify(args),
        boolResult getOrElse LBRACE))
      )
      if (boolResult.isEmpty) {
        indented(pin(result))
        p("\n" + ind(RBRACE))
      }
      result
    }
    else result
  }
}

object Tracer {
  def apply(enabled: => Boolean): Tracer = new Tracer(() => enabled)
}
