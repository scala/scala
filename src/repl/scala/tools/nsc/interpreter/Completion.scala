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
  case class Candidates(cursor: Int, candidates: List[String])
  val NoCandidates = Candidates(-1, Nil)

  // a leading dot plus something, but not ".." or "./", ignoring leading whitespace
  private val dotlike = """\s*\.[^./].*""".r
  def looksLikeInvocation(code: String) = code match {
    case null      => false   // insurance
    case dotlike() => true
    case _         => false
  }
}
