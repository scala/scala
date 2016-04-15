/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

import Completion._

/** An implementation-agnostic completion interface which makes no
 *  reference to the jline classes.
 */
trait Completion {
  def resetVerbosity(): Unit
  def complete(buffer: String, cursor: Int): Candidates
}
object NoCompletion extends Completion {
  def resetVerbosity() = ()
  def complete(buffer: String, cursor: Int) = NoCandidates
}

object Completion {
  case class Candidates(cursor: Int, candidates: List[String]) { }
  val NoCandidates = Candidates(-1, Nil)

  // a leading dot plus something, but not ".." or "./", ignoring leading whitespace
  private val dotlike = """\s*\.[^./].*""".r
  def looksLikeInvocation(code: String) = code match {
    case null      => false   // insurance
    case dotlike() => true
    case _         => false
  }
}
