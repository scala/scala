/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc.interpreter.shell

import Completion._

/** An implementation-agnostic completion interface which makes no
 *  reference to the jline classes.
 */
trait Completion {
  def resetVerbosity(): Unit
  def complete(buffer: String, cursor: Int): Candidates

  // Code accumulated in multi-line REPL input
  def partialInput: String = ""
  def withPartialInput[T](code: String)(body: => T): T = body
}
object NoCompletion extends Completion {
  def resetVerbosity() = ()
  def complete(buffer: String, cursor: Int) = NoCandidates
}

object Completion {
  case class Candidates(cursor: Int, candidates: List[String]) { }
  val NoCandidates = Candidates(-1, Nil)
}
