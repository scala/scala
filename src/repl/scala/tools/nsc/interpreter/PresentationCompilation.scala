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

import scala.reflect.internal.util.{Position, RangePosition, StringOps}
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
      val pc = newPresentationCompiler()
      val line1 = buf.patch(cursor, Cursor, 0)
      val trees = pc.newUnitParser(line1).parseStats()
      val importer = global.mkImporter(pc)
      val request = new Request(line1, trees map (t => importer.importTree(t)), generousImports = true)
      val origUnit = request.mkUnit
      val unit = new pc.CompilationUnit(origUnit.source)
      unit.body = pc.mkImporter(global).importTree(origUnit.body)
      import pc._
      val richUnit = new RichCompilationUnit(unit.source)
      // disable brace patching in the parser, the snippet template isn't well-indented and the results can be surprising
      currentRun.parsing.withIncompleteHandler((pos, msg) => ()) {
        unitOfFile(richUnit.source.file) = richUnit
        richUnit.body = unit.body
        enteringTyper(typeCheck(richUnit))
      }
      val inputRange = pc.wrappingPos(trees)
      // too bad dependent method types don't work for constructors
      val result = new PresentationCompileResult(pc, inputRange, cursor, buf) { val unit = richUnit ; override val compiler: pc.type = pc }
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
    def copySettings: Settings = {
      val s = new Settings(_ => () /* ignores "bad option -nc" errors, etc */)
      s.processArguments(global.settings.recreateArgs, processAll = false)
      s.YpresentationAnyThread.value = true
      s
    }
    val storeReporter: StoreReporter = new StoreReporter(copySettings)
    val interactiveGlobal = new interactive.Global(copySettings, storeReporter) { self =>
      def mergedFlatClasspath = {
        val replOutClasspath = ClassPathFactory.newClassPath(replOutput.dir, settings, closeableRegistry)
        AggregateClassPath(replOutClasspath :: global.platform.classPath :: Nil)
      }

      override lazy val platform: ThisPlatform = {
        new JavaPlatform {
          lazy val global: self.type = self
          override lazy val classPath: ClassPath = mergedFlatClasspath
        }
      }
    }
    new interactiveGlobal.TyperRun()
    interactiveGlobal
  }

  private var lastCommonPrefixCompletion: Option[String] = None

  abstract class PresentationCompileResult(val compiler: interactive.Global, val inputRange: Position, val cursor: Int, val buf: String) extends PresentationCompilationResult {
    val unit: compiler.RichCompilationUnit // depmet broken for constructors, can't be ctor arg

    override def cleanup(): Unit = {
      compiler.askShutdown()
    }
    import compiler.CompletionResult

    def completionsAt(cursor: Int): CompletionResult = compiler.completionsAt(positionOf(cursor))

    def positionOf(cursor: Int): Position =
      unit.source.position(cursor)

    def typedTreeAt(selectionStart: Int, selectionEnd: Int): compiler.Tree =
      compiler.typedTreeAt(new RangePosition(unit.source, selectionStart, selectionStart, selectionEnd))

    // offsets are 0-based
    def treeAt(start: Int, end: Int): compiler.Tree = treeAt(unit.source.position(start).withEnd(end))
    def treeAt(pos: Position): compiler.Tree = {
      import compiler.{Locator, Template, Block}
      new Locator(pos) locateIn unit.body match {
        case t@Template(_, _, constructor :: (rest :+ last)) =>
          if (rest.isEmpty) last
          else Block(rest, last)
        case t =>
          t
      }
    }

    def typeString(tree: compiler.Tree): String =
      compiler.exitingTyper(tree.tpe.toString)

    def treeString(tree: compiler.Tree): String =
      compiler.showCode(tree)

    override def print = {
      val tree = treeAt(inputRange)
      treeString(tree) + " // : " + tree.tpe.safeToString
    }


    override def typeAt(start: Int, end: Int) =
      typeString(typedTreeAt(start, end))

    val NoCandidates = (-1, Nil)
    type Candidates = (Int, List[String])

    override def candidates(tabCount: Int): Candidates = {
      import compiler._
      import CompletionResult.NoResults

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
            def isDeprecated(m: Member) = m match {
              case tm: TypeMember if tm.viaView.isDeprecated || tm.viaView != NoSymbol && tm.viaView.owner.isDeprecated || !m.sym.exists =>
                true
              case _ =>
                m.sym.isDeprecated ||
                m.sym.hasGetter && m.sym.getterIn(m.sym.owner).isDeprecated
            }
            (
              isUniversal && nme.isReplWrapperName(m.prefix.typeSymbol.name)
                || isUniversal && tabCount == 0 && r.name.isEmpty
                || isDeprecated(m) && tabCount == 0
                || viaUniversalExtensionMethod && tabCount == 0 && r.name.isEmpty
              )
          }

          val matching = r.matchingResults().filterNot(shouldHide)
          val tabAfterCommonPrefixCompletion = lastCommonPrefixCompletion.contains(buf.substring(inputRange.start, cursor)) && matching.exists(_.symNameDropLocal == r.name)
          val doubleTab = tabCount > 0 && matching.forall(_.symNameDropLocal == r.name)
          if (tabAfterCommonPrefixCompletion || doubleTab) {
            val pos1 = positionOf(cursor)
            import compiler._
            val locator = new Locator(pos1)
            val tree = locator locateIn unit.body
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
            }.traverse(unit.body)
            defStringCandidates(matching, r.name, isNew)
          } else if (matching.isEmpty) {
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
          Some(buf.substring(inputRange.start, found._1) + StringOps.longestCommonPrefix(found._2))
        else
          None
      found
    }

  }

}
