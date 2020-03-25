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
  def reset(): Unit

  def complete(buffer: String, cursor: Int): CompletionResult
}
object NoCompletion extends Completion {
  def reset() = ()
  def complete(buffer: String, cursor: Int) = NoCompletions
}

case class CompletionResult(cursor: Int, candidates: List[String]) {
  final def orElse(other: => CompletionResult): CompletionResult =
    if (candidates.nonEmpty) this else other
}
object CompletionResult {
  val empty: CompletionResult = NoCompletions
}
object NoCompletions extends CompletionResult(-1, Nil)

case class MultiCompletion(underlying: Completion*) extends Completion {
  override def reset() = underlying.foreach(_.reset())
  override def complete(buffer: String, cursor: Int) =
    underlying.foldLeft(CompletionResult.empty)((r,c) => r.orElse(c.complete(buffer, cursor)))
}
