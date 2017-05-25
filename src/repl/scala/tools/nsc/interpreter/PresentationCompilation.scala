/* NSC -- new Scala compiler
 * Copyright 2005-2015 LAMP/EPFL
 * @author Martin Odersky
 */
package scala.tools.nsc.interpreter

import scala.reflect.internal.util.{RangePosition, StringOps}
import scala.reflect.io.AbstractFile
import scala.tools.nsc.backend.JavaPlatform
import scala.tools.nsc.util.ClassPath
import scala.tools.nsc.{Settings, interactive}
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.classpath._
import scala.tools.nsc.interpreter.Results.{Error, Result}

trait PresentationCompilation { self: IMain =>

  private final val Cursor = IMain.DummyCursorFragment + " "

  /** Typecheck a line of REPL input, suitably wrapped with "interpreter wrapper" objects/classes, with the
    * presentation compiler. The result of this method gives access to the typechecked tree and to autocompletion
    * suggestions.
    *
    * The caller is responsible for calling [[PresentationCompileResult#cleanup]] to dispose of the compiler instance.
    */
  def presentationCompile(cursor: Int, buf: String): Either[Result, PresentationCompileResult] = {
    if (global == null) Left(Error)
    else {
      val compiler = newPresentationCompiler()
      val line1 = buf.patch(cursor, Cursor, 0)
      val trees = compiler.newUnitParser(line1).parseStats()
      val importer = global.mkImporter(compiler)
      val request = new Request(line1, trees map (t => importer.importTree(t)), generousImports = true)
      val wrappedCode: String = request.ObjectSourceCode(request.handlers)
      val unit = compiler.newCompilationUnit(wrappedCode)
      import compiler._
      val richUnit = new RichCompilationUnit(unit.source)
      unitOfFile(richUnit.source.file) = richUnit
      enteringTyper(typeCheck(richUnit))
      val result = PresentationCompileResult(compiler)(cursor, buf, richUnit, request.ObjectSourceCode.preambleLength)
      Right(result)
    }
  }

  /** Create an instance of the presentation compiler with a classpath comprising the REPL's configured classpath
    * and the classes output by previously compiled REPL lines.
    *
    * You may directly interact with this compiler from any thread, although you must not access it concurrently
    * from multiple threads.
    *
    * You may downcast the `reporter` to `StoreReporter` to access type errors.
    */
  def newPresentationCompiler(): interactive.Global = {
    def mergedFlatClasspath = {
      val replOutClasspath = ClassPathFactory.newClassPath(replOutput.dir, settings)
      AggregateClassPath(replOutClasspath :: global.platform.classPath :: Nil)
    }
    def copySettings: Settings = {
      val s = new Settings(_ => () /* ignores "bad option -nc" errors, etc */)
      s.processArguments(global.settings.recreateArgs, processAll = false)
      s.YpresentationAnyThread.value = true
      s
    }
    val storeReporter: StoreReporter = new StoreReporter
    val interactiveGlobal = new interactive.Global(copySettings, storeReporter) { self =>
      override lazy val platform: ThisPlatform = {
        new JavaPlatform {
          lazy val global: self.type = self
          override private[nsc] lazy val classPath: ClassPath = mergedFlatClasspath
        }
      }
    }
    new interactiveGlobal.TyperRun()
    interactiveGlobal
  }

  private var lastCommonPrefixCompletion: Option[String] = None

  abstract class PresentationCompileResult extends PresentationCompilationResult {
    private[interpreter] val compiler: scala.tools.nsc.interactive.Global
    def unit: compiler.RichCompilationUnit
    /** The length of synthetic code the precedes the user written code */
    def preambleLength: Int
    override def cleanup(): Unit = {
      compiler.askShutdown()
    }
    import compiler.CompletionResult

    def completionsAt(cursor: Int): CompletionResult = {
      val pos = unit.source.position(preambleLength + cursor)
      compiler.completionsAt(pos)
    }
    def typedTreeAt(code: String, selectionStart: Int, selectionEnd: Int): compiler.Tree = {
      val start = selectionStart + preambleLength
      val end   = selectionEnd + preambleLength
      val pos   = new RangePosition(unit.source, start, start, end)
      compiler.typedTreeAt(pos)
    }

    def tree: compiler.Tree = {
      import compiler.{Locator, Template, Block}
      val offset = preambleLength
      val pos1 = unit.source.position(offset).withEnd(offset + buf.length)
      new Locator(pos1) locateIn unit.body match {
        case Template(_, _, constructor :: preambleEndMember :: (rest :+ last)) => if (rest.isEmpty) last else Block(rest, last)
        case t => t
      }
    }

    def typeString(tree: compiler.Tree): String =
      compiler.exitingTyper(tree.tpe.toString)

    def treeString(tree: compiler.Tree): String =
      compiler.showCode(tree)

    override def print = {
      val tree1 = tree
      treeString(tree1) + " // : " + tree1.tpe.safeToString
    }


    override def typeAt(start: Int, end: Int) = {
      typeString(typedTreeAt(buf, start, end))
    }

    val NoCandidates = (-1, Nil)
    type Candidates = (Int, List[String])

    override def candidates(tabCount: Int): Candidates = {
      import compiler._
      import CompletionResult.NoResults

      def defStringCandidates(matching: List[Member], name: Name): Candidates = {
        val defStrings = for {
          member <- matching
          if member.symNameDropLocal == name
          sym <- member.sym.alternatives
          sugared = sym.sugaredSymbolOrSelf
        } yield {
          val tp = member.prefix memberType sym
          sugared.defStringSeenAs(tp)
        }
        (cursor, "" :: defStrings.distinct)
      }
      val found = this.completionsAt(cursor) match {
        case NoResults => NoCandidates
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
          if (tabAfterCommonPrefixCompletion || doubleTab) defStringCandidates(matching, r.name)
          else if (matching.isEmpty) {
            // Lenient matching based on camel case and on eliding JavaBean "get" / "is" boilerplate
            val camelMatches: List[Member] = r.matchingResults(CompletionResult.camelMatch(_)).filterNot(shouldHide)
            val memberCompletions = camelMatches.map(_.symNameDropLocal.decoded).distinct.sorted
            def allowCompletion = (
              (memberCompletions.size == 1)
                || CompletionResult.camelMatch(r.name)(r.name.newName(StringOps.longestCommonPrefix(memberCompletions)))
              )
            if (memberCompletions.isEmpty) NoCandidates
            else if (allowCompletion) (cursor - r.positionDelta, memberCompletions)
            else (cursor, "" :: memberCompletions)
          } else if (matching.nonEmpty && matching.forall(_.symNameDropLocal == r.name))
            NoCandidates // don't offer completion if the only option has been fully typed already
          else {
            // regular completion
            val memberCompletions: List[String] = matching.map(_.symNameDropLocal.decoded).distinct.sorted
            (cursor - r.positionDelta, memberCompletions)
          }
      }
      lastCommonPrefixCompletion =
        if (found != NoCandidates && buf.length >= found._1)
          Some(buf.substring(0, found._1) + StringOps.longestCommonPrefix(found._2))
        else
          None
      found
    }

  }

  object PresentationCompileResult {
    def apply(compiler0: interactive.Global)(cursor0: Int, buf0: String, unit0: compiler0.RichCompilationUnit, preambleLength0: Int) = new PresentationCompileResult {
      def cursor = cursor0
      def buf = buf0

      override val compiler = compiler0

      override def unit = unit0.asInstanceOf[compiler.RichCompilationUnit]

      override def preambleLength = preambleLength0
    }
  }
}
