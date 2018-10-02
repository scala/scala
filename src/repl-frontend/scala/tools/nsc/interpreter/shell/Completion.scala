/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.interpreter.shell

trait Completion {
  def resetVerbosity(): Unit

  def complete(buffer: String, cursor: Int): CompletionResult

  // Code accumulated in multi-line REPL input
  def partialInput: String = ""
  def withPartialInput[T](code: String)(body: => T): T = body
}
object NoCompletion extends Completion {
  def resetVerbosity() = ()
  def complete(buffer: String, cursor: Int) = NoCompletions
}

case class CompletionResult(cursor: Int, candidates: List[String])
object NoCompletions extends CompletionResult(-1, Nil)
