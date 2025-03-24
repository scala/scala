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

package scala.tools.nsc.interpreter
package shell

trait Completion {
  final def complete(buffer: String, cursor: Int): CompletionResult = complete(buffer, cursor, filter = true)
  def complete(buffer: String, cursor: Int, filter: Boolean): CompletionResult
}
object NoCompletion extends Completion {
  def complete(buffer: String, cursor: Int, filter: Boolean) = NoCompletions
}

case class CompletionResult(line: String, cursor: Int, candidates: List[CompletionCandidate], typeAtCursor: String = "", typedTree: String = "") {
  final def orElse(other: => CompletionResult): CompletionResult =
    if (candidates.nonEmpty) this else other
}
object CompletionResult {
  val empty: CompletionResult = NoCompletions
}
object NoCompletions extends CompletionResult("", -1, Nil, "", "")

case class MultiCompletion(underlying: Completion*) extends Completion {
  override def complete(buffer: String, cursor: Int, filter: Boolean) =
    underlying.foldLeft(CompletionResult.empty)((r,c) => r.orElse(c.complete(buffer, cursor, filter)))
}
