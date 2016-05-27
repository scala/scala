package scala.tools.nsc
package interpreter

import scala.runtime.ScalaRunTime

trait ReplPrinter[-A] extends Any {
  def print(x: A, maxElements: Int): String
}

object ReplPrinter {
  implicit def default[A]: ReplPrinter[A] = new ReplPrinter[A] {
    def print(x: A, maxElements: Int) = ScalaRunTime.replStringOf(x, maxElements)
  }
}
