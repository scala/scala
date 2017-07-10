/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
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
