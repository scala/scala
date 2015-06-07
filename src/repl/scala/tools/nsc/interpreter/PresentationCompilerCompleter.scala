/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Martin Odersky
 */
package scala.tools.nsc.interpreter

import scala.tools.nsc.interpreter.Completion.{ScalaCompleter, Candidates}

class PresentationCompilerCompletionProvider(intp: IMain) extends Completion {
  def resetVerbosity(): Unit = ()
  def completer(): ScalaCompleter = new PresentationCompilerCompleter(intp)
}

class PresentationCompilerCompleter(intp: IMain) extends ScalaCompleter {
  private case class Request(line: String, cursor: Int)
  private var lastRequest = new Request("", -1)
  private var tabCount = 0

  override def complete(buf: String, cursor: Int): Candidates = {
    val request = new Request(buf, cursor)
    if (request == lastRequest) tabCount += 1 else { tabCount = 0; lastRequest = request}
    val printMode = buf.matches(""".*// *print *$""") && cursor == buf.length
    val (typeAtMode, start, end) = {
      val R = """.*// *typeAt *(\d+) *(\d+) *$""".r
      buf match {
        case R(s, e) if cursor == buf.length => (true, s.toInt, e.toInt)
        case _ => (false, -1, -1)
      }
    }
    intp.presentationCompile(buf) match {
      case Left(_) => Completion.NoCandidates
      case Right(result) => try {
        if (printMode) {
          val offset = result.preambleLength
          val pos1 = result.unit.source.position(offset).withEnd(offset + buf.length)
          import result.compiler._
          val tree = new Locator(pos1) locateIn result.unit.body match {
            case Template(_, _, constructor :: (rest :+ last)) => if (rest.isEmpty) last else Block(rest, last)
            case t => t
          }
          val printed = showCode(tree)
          Candidates(cursor, "" :: printed :: Nil)
        } else if (typeAtMode) {
          val tp = intp.api.typeAt(buf, start, end)
          Candidates(cursor, "" :: tp ++: Nil)
        } else {
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
      } finally result.cleanup()
    }
  }
}
