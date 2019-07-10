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

import scala.reflect.internal.util.StringOps
import scala.tools.nsc.interpreter.Completion.Candidates
import scala.util.control.NonFatal

class PresentationCompilerCompleter(intp: IMain) extends Completion {
  import PresentationCompilerCompleter._
  import intp.{PresentationCompileResult => Result}

  private type Handler = Result => Candidates

  private var lastRequest = NoRequest
  private var tabCount = 0
  private var lastCommonPrefixCompletion: Option[String] = None

  def resetVerbosity(): Unit = { tabCount = 0 ; lastRequest = NoRequest }

  // A convenience for testing
  def complete(before: String, after: String = ""): Candidates = complete(before + after, before.length)
  override def complete(buf: String, cursor: Int): Candidates = {
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
    val Cursor = IMain.DummyCursorFragment + " "

    def print(result: Result) = {
      val offset = result.preambleLength
      val pos1 = result.unit.source.position(offset).withEnd(offset + buf.length)
      import result.compiler._
      val tree = new Locator(pos1) locateIn result.unit.body match {
        case Template(_, _, constructor :: (rest :+ last)) => if (rest.isEmpty) last else Block(rest, last)
        case t => t
      }
      val printed = showCode(tree) + " // : " + tree.tpe.safeToString
      Candidates(cursor, "" :: printed :: Nil)
    }
    def typeAt(result: Result, start: Int, end: Int) = {
      val tpString = result.compiler.exitingTyper(result.typedTreeAt(start, end).tpe.toString)
      Candidates(cursor, "" :: tpString :: Nil)
    }
    def candidates(result: Result): Candidates = {
      import result.compiler._
      import CompletionResult._
      def defStringCandidates(matching: List[Member], name: Name, isNew: Boolean): Candidates = {
        val defStrings = for {
          member <- matching
          if member.symNameDropLocal == name
          sym <- if (member.sym.isClass && isNew) member.sym.info.decl(nme.CONSTRUCTOR).alternatives else member.sym.alternatives
          sugared = sym.sugaredSymbolOrSelf
        } yield {
            val tp = member.prefix memberType sym
            sugared.defStringSeenAs(tp)
          }
        Candidates(cursor, "" :: defStrings.distinct)
      }
      val found = result.completionsAt(cursor) match {
        case NoResults => Completion.NoCandidates
        case r =>
          def shouldHide(m: Member): Boolean = {
            val isUniversal = definitions.isUniversalMember(m.sym)
            def viaUniversalExtensionMethod = m match {
              case t: TypeMember if t.implicitlyAdded && t.viaView.info.params.head.info.bounds.isEmptyBounds => true
              case _ => false
            }
            (
                 isUniversal && nme.isReplWrapperName(m.prefix.typeSymbol.name)
              || isUniversal && tabCount == 0 && r.name.isEmpty
              || viaUniversalExtensionMethod && tabCount == 0 && r.name.isEmpty
            )
          }

          val matching = r.matchingResults().filterNot(shouldHide)
          val tabAfterCommonPrefixCompletion = lastCommonPrefixCompletion.contains(buf.substring(0, cursor)) && matching.exists(_.symNameDropLocal == r.name)
          val doubleTab = tabCount > 0 && matching.forall(_.symNameDropLocal == r.name)
          if (tabAfterCommonPrefixCompletion || doubleTab) {
            val offset = result.preambleLength
            val pos1 = result.positionOf(cursor)
            import result.compiler._
            val locator = new Locator(pos1)
            val tree = locator locateIn result.unit.body
            var isNew = false
            new TreeStackTraverser {
              override def traverse(t: Tree): Unit = {
                if (t eq tree) {
                  isNew = path.dropWhile { case _: Select | _: Annotated => true; case _ => false}.headOption match {
                    case Some(_: New) => true
                    case _ => false
                  }
                } else super.traverse(t)
              }
            }.traverse(result.unit.body)
            defStringCandidates(matching, r.name, isNew)
          } else if (matching.isEmpty) {
            // Lenient matching based on camel case and on eliding JavaBean "get" / "is" boilerplate
            val camelMatches: List[Member] = r.matchingResults(CompletionResult.camelMatch(_)).filterNot(shouldHide)
            val memberCompletions = camelMatches.map(_.symNameDropLocal.decoded).distinct.sorted
            def allowCompletion = (
                 (memberCompletions.size == 1)
              || CompletionResult.camelMatch(r.name)(r.name.newName(StringOps.longestCommonPrefix(memberCompletions)))
            )
            if (memberCompletions.isEmpty) Completion.NoCandidates
            else if (allowCompletion) Candidates(cursor - r.positionDelta, memberCompletions)
            else Candidates(cursor, "" :: memberCompletions)
          } else if (matching.nonEmpty && matching.forall(_.symNameDropLocal == r.name))
            Completion.NoCandidates // don't offer completion if the only option has been fully typed already
          else {
            // regular completion
            val memberCompletions: List[String] = matching.map(_.symNameDropLocal.decoded).distinct.sorted
            Candidates(cursor - r.positionDelta, memberCompletions)
          }
      }
      lastCommonPrefixCompletion =
        if (found != Completion.NoCandidates && buf.length >= found.cursor)
          Some(buf.substring(0, found.cursor) + StringOps.longestCommonPrefix(found.candidates))
        else
          None
      found
    }
    val buf1 = buf.patch(cursor, Cursor, 0)
    try {
      intp.presentationCompile(buf1) match {
        case Left(_) => Completion.NoCandidates
        case Right(result) => try {
          buf match {
            case slashPrint() if cursor == buf.length =>
              val c = print(result)
              c.copy(candidates = c.candidates.map(intp.naming.unmangle))
            case slashPrintRaw() if cursor == buf.length => print(result)
            case slashTypeAt(start, end) if cursor == buf.length => typeAt(result, start.toInt, end.toInt)
            case _ => candidates(result)
          }
        } finally result.cleanup()
      }
    } catch {
      case NonFatal(e) =>
        if (isReplDebug) e.printStackTrace()
        Completion.NoCandidates
    }
  }
}
object PresentationCompilerCompleter {
  private case class Request(line: String, cursor: Int)
  private val NoRequest = Request("", -1)
}
