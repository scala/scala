/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL, Typesafe Inc.
 * @author  Adriaan Moors
 */

package scala
package tools.nsc

import scala.reflect.internal.Positions

/** Similar to Reporting: gather global functionality specific to parsing.
 */
trait Parsing { self : Positions with Reporting =>
  def currentRun: RunParsing

  trait RunParsing {
    val parsing: PerRunParsing = new PerRunParsing
  }

  class PerRunParsing {
    // for repl
    private[this] var incompleteHandler: (Position, String) => Unit = null
    def withIncompleteHandler[T](handler: (Position, String) => Unit)(thunk: => T) = {
      val saved = incompleteHandler
      incompleteHandler = handler
      try thunk
      finally incompleteHandler = saved
    }

    def incompleteHandled = incompleteHandler != null
    def incompleteInputError(pos: Position, msg: String): Unit =
      if (incompleteHandled) incompleteHandler(pos, msg)
      else reporter.error(pos, msg)
  }
}