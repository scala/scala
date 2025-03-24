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

import scala.util.control.NonFatal

/** Completion for the REPL.
 */
class ReplCompletion(intp: Repl, val accumulator: Accumulator = new Accumulator) extends Completion {

  def complete(buffer: String, cursor: Int, filter: Boolean): CompletionResult = {
    // special case for:
    //
    // scala> 1
    // scala> .toInt
    val bufferWithVar =
      if (Parsed.looksLikeInvocation(buffer)) intp.mostRecentVar + buffer
      else buffer

    val bufferWithMultiLine = accumulator.toString + bufferWithVar
    val cursor1 = cursor + (bufferWithMultiLine.length - buffer.length)
    codeCompletion(bufferWithMultiLine, cursor1, filter)
  }

  // A convenience for testing
  def complete(before: String, after: String = ""): CompletionResult = complete(before + after, before.length)

  private def codeCompletion(buf: String, cursor: Int, filter: Boolean): CompletionResult = {
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
              CompletionResult(buf, cursor, CompletionCandidate.fromStrings("" :: Naming.unmangle(result.print) :: Nil), "", "")
            case slashPrintRaw() if cursor == buf.length         =>
              CompletionResult(buf, cursor, CompletionCandidate.fromStrings("" :: result.print :: Nil), "", "")
            case slashTypeAt(start, end) if cursor == buf.length =>
              CompletionResult(buf, cursor, CompletionCandidate.fromStrings("" :: result.typeAt(start.toInt, end.toInt) :: Nil), "", "")
            case _                                               =>
              // under JLine 3, we no longer use the tabCount concept, so tabCount is always 1
              // which always gives us all completions
              val (c, r)                         = result.completionCandidates(filter, tabCount = 1)
              val typeAtCursor = result.typeAt(cursor, cursor)
              CompletionResult(buf, c, r, typeAtCursor, result.print)
          }
        } finally result.cleanup()
      }
    } catch {
      case NonFatal(e) =>
        if (intp.settings.debug.value)
          e.printStackTrace()
        NoCompletions
    }
  }
}
