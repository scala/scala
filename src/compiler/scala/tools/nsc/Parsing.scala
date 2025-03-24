/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package tools.nsc

import scala.reflect.internal.Positions
import scala.reflect.internal.util.CodeAction

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
    def incompleteInputError(pos: Position, msg: String, actions: List[CodeAction] = Nil): Unit =
      if (incompleteHandled) incompleteHandler(pos, msg)
      else reporter.error(pos, msg, actions)
  }
}
