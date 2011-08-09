/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package util

import java.io.PrintStream
import scala.runtime.ScalaRunTime

class Tracer(enabled: () => Boolean) {
  def out: PrintStream = System.out
  def stringify(x: Any) = ScalaRunTime stringOf x

  // So can pass tuples, lists, whatever as arguments and don't
  // get a lot of extra parens or noisy prefixes.
  def stringifyArgs(x: Any) = {
    x match {
      case x: TraversableOnce[_] => x map stringify mkString ", "
      case x: Product            => x.productIterator map stringify mkString ", "
      case _                     => stringify(x)
    }
  }

  private val LBRACE = "{"
  private val RBRACE = "}"
  private var indentLevel = 0
  private def spaces = " " * (indentLevel * 2)
  private def pblock(result: Any) = {
    p(LBRACE + "\n")
    indented(p(spaces + stringify(result) + "\n"))
    p(spaces + RBRACE + "\n")
  }
  private def passign(name: String, args: String) =
    p(spaces + name + "(" + args + ") = ")

  private def indented[T](body: => T): T = {
    indentLevel += 1
    try body
    finally indentLevel -= 1
  }
  private def p(s: String) = {
    out.print(s)
    out.flush()
  }

  def apply[T](name: String, args: => Any)(body: => T): T = {
    val result = body

    if (enabled()) {
      passign(name, stringifyArgs(args))
      val resultToPrint = result match {
        case Some(x)  => x
        case _        => result
      }
      // concise output optimization
      val isOneliner = resultToPrint match {
        case _: Boolean | _: None.type => true
        case s: String                 => s.length < 40
        case _                         => false
      }
      if (isOneliner) p(stringify(resultToPrint) + "\n")
      else pblock(resultToPrint)
    }

    result
  }
}

object Tracer {
  def apply(enabled: => Boolean): Tracer = new Tracer(() => enabled)
}
