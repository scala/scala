/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Martin Odersky
 */
package scala.tools.nsc.interpreter

import scala.tools.nsc.interpreter.Completion.{ScalaCompleter, Candidates}

class PresentationCompilerCompleter(intp: IMain) extends Completion with ScalaCompleter {
  import PresentationCompilerCompleter._
  import intp.{PresentationCompileResult => Result}

  private type Handler = Result => Candidates

  private var lastRequest = NoRequest
  private var tabCount = 0

  def resetVerbosity(): Unit = { tabCount = 0 ; lastRequest = NoRequest }
  def completer(): ScalaCompleter = this

  override def complete(buf: String, cursor: Int): Candidates = {
    val request = Request(buf, cursor)
    if (request == lastRequest) tabCount += 1 else { tabCount = 0 ; lastRequest = request }

    // secret handshakes
    val slashPrint  = """.*// *print *""".r
    val slashTypeAt = """.*// *typeAt *(\d+) *(\d+) *""".r

    def print: Handler = { result =>
      val offset = result.preambleLength
      val pos1 = result.unit.source.position(offset).withEnd(offset + buf.length)
      import result.compiler._
      val tree = new Locator(pos1) locateIn result.unit.body match {
        case Template(_, _, constructor :: (rest :+ last)) => if (rest.isEmpty) last else Block(rest, last)
        case t => t
      }
      val printed = showCode(tree)
      Candidates(cursor, "" :: printed :: Nil)
    }
    def typeAt(start: Int, end: Int): Handler = { result =>
      val tp = intp.api.typeAt(buf, start, end)
      Candidates(cursor, "" :: tp ++: Nil)
    }
    def candidates: Handler = { result =>
      import result.CompletionResult._
      result.completionsOf(buf, cursor) match {
        case TypeMembers(newCursor, result.compiler.Select(qual, name), members) =>
          if (tabCount > 0 && members.forall(_.sym.name == name)) {
            val defStrings = members.flatMap(_.sym.alternatives).map(sym => sym.defStringSeenAs(qual.tpe memberType sym))
            Candidates(cursor, "" :: defStrings)
          } else {
            val memberCompletions: List[String] = members.map(_.sym.name.decoded)
            Candidates(newCursor, memberCompletions)
          }
        case ScopeMembers(newCursor, members) =>
          Candidates(newCursor, members.map(_.sym.name.decoded))
        case _ =>
          Completion.NoCandidates
      }
    }
    val handler: Handler = buf match {
      case slashPrint()            if cursor == buf.length => print
      case slashTypeAt(start, end) if cursor == buf.length => typeAt(start.toInt, end.toInt)
      case _                                               => candidates
    }
    intp.presentationCompile(buf) match {
      case Left(_)       => Completion.NoCandidates
      case Right(result) => try handler(result) finally result.cleanup()
    }
  }
}
object PresentationCompilerCompleter {
  private case class Request(line: String, cursor: Int)
  private val NoRequest = Request("", -1)
}
