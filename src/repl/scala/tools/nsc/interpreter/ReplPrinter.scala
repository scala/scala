package scala.tools.nsc
package interpreter

import scala.runtime.ScalaRunTime

trait ReplPrinter[-A] extends Any {
  def print(x: A, maxElements: Int): String
}

object ReplPrinter {
  implicit object defaultReplPrinter extends ReplPrinter[Any] {
    def print(x: Any, maxElements: Int) = ScalaRunTime.replStringOf(x, maxElements)
  }
}
