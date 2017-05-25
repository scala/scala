/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Martin Odersky
 */
package scala.tools.nsc.interpreter.shell

import scala.util.control.NonFatal
import Completion.Candidates
import scala.tools.nsc.interpreter.Repl

class PresentationCompilerCompleter(intp: Repl) extends Completion {
  import PresentationCompilerCompleter._

  private var lastRequest = NoRequest
  private var tabCount = 0

  def resetVerbosity(): Unit = { tabCount = 0 ; lastRequest = NoRequest }

  // A convenience for testing
  def complete(before: String, after: String = ""): Candidates = complete(before + after, before.length)
  override def complete(buf: String, cursor: Int): Candidates = {
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
    val slashTypeAt = """.*// *typeAt *(\d+) *(\d+) *""".r
    try {
      intp.presentationCompile(cursor, buf) match {
        case Left(_) => Completion.NoCandidates
        case Right(result) => try {
          buf match {
            case slashPrint() if cursor == buf.length => Candidates(cursor, "" :: result.print :: Nil)
            case slashTypeAt(start, end) if cursor == buf.length => Candidates(cursor, "" :: result.typeAt(start.toInt, end.toInt)  :: Nil)
            case _ => val (c, r) = result.candidates(tabCount); Candidates(c, r)
          }
        } finally result.cleanup()
      }
    } catch {
      case NonFatal(e) =>
        // e.printStackTrace()
        Completion.NoCandidates
    }
  }
}
object PresentationCompilerCompleter {
  private case class Request(line: String, cursor: Int)
  private val NoRequest = Request("", -1)
}
