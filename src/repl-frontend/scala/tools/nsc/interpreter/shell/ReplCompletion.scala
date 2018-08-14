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

import scala.util.control.NonFatal
import scala.tools.nsc.interpreter.Repl
import scala.tools.nsc.interpreter.Naming

/** Completion for the REPL.
 */
class ReplCompletion(intp: Repl, val accumulator: Accumulator) extends Completion {
  import ReplCompletion._

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

  private var lastRequest = NoRequest
  private var tabCount = 0

  def reset(): Unit = { tabCount = 0 ; lastRequest = NoRequest }

  // A convenience for testing
  def complete(before: String, after: String = ""): CompletionResult = complete(before + after, before.length)

  private def codeCompletion(buf: String, cursor: Int): CompletionResult = {
    require(cursor >= 0 && cursor <= buf.length)

    val request = Request(buf, cursor)
    if (request == lastRequest)
      tabCount += 1
    else {
      tabCount = 0
      lastRequest = request
    }

    // secret handshakes
    val slashPrint  = """.*// *print *""".r
    val slashPrintRaw  = """.*// *printRaw *""".r
    val slashTypeAt = """.*// *typeAt *(\d+) *(\d+) *""".r
    try {
      intp.presentationCompile(cursor, buf) match {
        case Left(_) => NoCompletions
        case Right(result) => try {
          buf match {
            case slashPrint() if cursor == buf.length => CompletionResult(cursor, "" :: Naming.unmangle(result.print) :: Nil)
            case slashPrintRaw() if cursor == buf.length => CompletionResult(cursor, "" :: result.print :: Nil)
            case slashTypeAt(start, end) if cursor == buf.length => CompletionResult(cursor, "" :: result.typeAt(start.toInt, end.toInt) :: Nil)
            case _ => val (c, r) = result.candidates(tabCount); CompletionResult(c, r)
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

object ReplCompletion {
  private case class Request(line: String, cursor: Int)
  private val NoRequest = Request("", -1)
}
