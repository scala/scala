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

package scala.tools.nsc.interpreter
package shell

import scala.util.control.NonFatal

/** Completion for the REPL.
 */
class ReplCompletion(intp: Repl, val accumulator: Accumulator = new Accumulator) extends Completion {

  def complete(buffer: String, cursor: Int): CompletionResult = {
    // special case for:
    //
    // scala> 1
    // scala> .toInt
    val bufferWithVar =
      if (Parsed.looksLikeInvocation(buffer)) intp.mostRecentVar + buffer
      else buffer

    val bufferWithMultiLine = accumulator.toString + bufferWithVar
    val cursor1 = cursor + (bufferWithMultiLine.length - buffer.length)
    codeCompletion(bufferWithMultiLine, cursor1)
  }

  // A convenience for testing
  def complete(before: String, after: String = ""): CompletionResult = complete(before + after, before.length)

  private def codeCompletion(buf: String, cursor: Int): CompletionResult = {
    require(cursor >= 0 && cursor <= buf.length)

    // secret handshakes
    val slashPrint  = """.*// *print *""".r
    val slashPrintRaw  = """.*// *printRaw *""".r
    val slashTypeAt = """.*// *typeAt *(\d+) *(\d+) *""".r
    try {
      intp.presentationCompile(cursor, buf) match {
        case Left(_) => NoCompletions
        case Right(result) => try {
          buf match {
            case slashPrint() if cursor == buf.length            =>
              CompletionResult(buf, cursor, CompletionCandidate.fromStrings("" :: Naming.unmangle(result.print) :: Nil))
            case slashPrintRaw() if cursor == buf.length         =>
              CompletionResult(buf, cursor, CompletionCandidate.fromStrings("" :: result.print :: Nil))
            case slashTypeAt(start, end) if cursor == buf.length =>
              CompletionResult(buf, cursor, CompletionCandidate.fromStrings("" :: result.typeAt(start.toInt, end.toInt) :: Nil))
            case _                                               =>
              // under JLine 3, we no longer use the tabCount concept, so tabCount is always 1
              // which always gives us all completions
              val (c, r)                         = result.completionCandidates(tabCount = 1)
              // scala/bug#12238
              // Currently, only when all methods are Deprecated should they be displayed `Deprecated` to users. Only handle result of PresentationCompilation#toCandidates.
              // We don't handle result of PresentationCompilation#defStringCandidates, because we need to show the deprecated here.
              if (r.nonEmpty && r.forall(!_.defString.startsWith("def"))) {
                val groupByDef              = r.groupBy(_.defString)
                val allOverrideIsUniversal  = groupByDef.filter(f => f._2.forall(_.isUniversal)).keySet
                val allOverrideIsDeprecated = groupByDef.filter(f => f._2.forall(_.isDeprecated)).keySet
                def isOverrideMethod(candidate: CompletionCandidate): Boolean = groupByDef(candidate.defString).size > 1
                val rewriteDecr = r.map(candidate => {
                  // If not all overloaded methods are deprecated, but they are overloaded methods, they (all) should be set to false.
                  val isUniv = if (!allOverrideIsUniversal.contains(candidate.defString) && isOverrideMethod(candidate)) false else candidate.isUniversal
                  val isDepr = if (!allOverrideIsDeprecated.contains(candidate.defString) && isOverrideMethod(candidate)) false else candidate.isDeprecated
                  candidate.copy(isUniversal = isUniv, isDeprecated = isDepr)
                })
                CompletionResult(buf, c, rewriteDecr)
              } else CompletionResult(buf, c, r)
          }
        } finally result.cleanup()
      }
    } catch {
      case NonFatal(e) =>
        // e.printStackTrace()
        NoCompletions
    }
  }
}
