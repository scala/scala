/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package util

class Tracer(enabled: () => Boolean) {
  private var indentLevel = 0
  private def ind(s: String) = (" " * (indentLevel*2)) + s
  private def indented[T](body: => T): T = {
    indentLevel += 1
    try body
    finally indentLevel -= 1
  }
  private def p(s: String) = {
    System.out.print(s)
    System.out.flush()
  }
  private def pin[T](x: T): T = {
    p(ind("" + x))
    x
  }
  def seq[T](name: String, args: => Seq[Any])(body: => T): T = {
    if (enabled()) {
      p(ind("%s(%s) = {\n".format(name, args mkString ", ")))
      try indented(pin(body))
      finally println("\n" + ind("}"))
    }
    else body
  }
  def apply[T](name: String, args: Any*)(body: => T): T = seq(name, args.toSeq)(body)
}

object Tracer {
  def apply(enabled: => Boolean): Tracer = new Tracer(() => enabled)
}
