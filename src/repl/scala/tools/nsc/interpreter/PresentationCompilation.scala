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
  def presentationCompile(cursor: Int, buf: String): Either[Result, PresentationCompilationResult] = {
    if (global == null) Left(Error)
    else {
      val pc = newPresentationCompiler()
      val line1 = buf.patch(cursor, Cursor, 0)
      val trees = pc.newUnitParser(line1).parseStats()
      val importer = global.mkImporter(pc)
      //println(s"pc: [[$line1]], <<${trees.size}>>")
      val request = new Request(line1, trees map (t => importer.importTree(t)), generousImports = true)
      val origUnit = request.mkUnit
      val unit = new pc.CompilationUnit(origUnit.source)
      unit.body = pc.mkImporter(global).importTree(origUnit.body)
      val richUnit = new pc.RichCompilationUnit(unit.source)
      // disable brace patching in the parser, the snippet template isn't well-indented and the results can be surprising
      pc.currentRun.parsing.withIncompleteHandler((pos, msg) => ()) {
        pc.unitOfFile(richUnit.source.file) = richUnit
        richUnit.body = unit.body
        pc.enteringTyper(pc.typeCheck(richUnit))
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
    type Candidates = (Int, List[CompletionCandidate])

    override def completionCandidates(tabCount: Int): Candidates = {
      import compiler._
      import CompletionResult.NoResults

      def isMemberDeprecated(m: Member) = m match {
        case tm: TypeMember if tm.viaView.isDeprecated || tm.viaView != NoSymbol && tm.viaView.owner.isDeprecated =>
          true
        case _ =>
          m.sym.isDeprecated ||
            m.sym.hasGetter && m.sym.getterIn(m.sym.owner).isDeprecated
      }
      def isMemberUniversal(m: Member) =
        definitions.isUniversalMember(m.sym) ||
          (m match {
            case t: TypeMember =>
              t.implicitlyAdded && t.viaView.info.params.head.info.bounds.isEmptyBounds
            case _ =>
              false
          })
      def memberArity(m: Member): CompletionCandidate.Arity =
        if (m.sym.paramss.isEmpty) CompletionCandidate.Nullary
        else if (m.sym.paramss.size == 1 && m.sym.paramss.head.isEmpty) CompletionCandidate.Nilary
        else CompletionCandidate.Other
      def defStringCandidates(matching: List[Member], name: Name, isNew: Boolean): Candidates = {
        val ccs = for {
          member <- matching
          if member.symNameDropLocal == name
          sym <- if (member.sym.isClass && isNew) member.sym.info.decl(nme.CONSTRUCTOR).alternatives else member.sym.alternatives
          sugared = sym.sugaredSymbolOrSelf
        } yield {
          val tp         = member.prefix memberType sym
          val desc       = Seq(if (isMemberDeprecated(member)) "(deprecated)" else "", if (isMemberUniversal(member)) "(universal)" else "")
          val methodOtherDesc = if (!desc.exists(_ != "")) "" else " " + desc.filter(_ != "").mkString(" ")
          CompletionCandidate(
            defString = sugared.defStringSeenAs(tp) + methodOtherDesc,
            arity = memberArity(member),
            isDeprecated = isMemberDeprecated(member),
            isUniversal = isMemberUniversal(member))
        }
        (cursor, CompletionCandidate("") :: ccs.distinct)
      }
      def toCandidates(members: List[Member]): List[CompletionCandidate] =
        members
          .map(m => CompletionCandidate(m.symNameDropLocal.decoded, memberArity(m), isMemberDeprecated(m), isMemberUniversal(m)))
          .sortBy(_.defString)
      val found = this.completionsAt(cursor) match {
        case NoResults => NoCandidates
        case r =>
          def shouldHide(m: Member): Boolean =
            tabCount == 0 && (isMemberDeprecated(m) || isMemberUniversal(m))
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
            val memberCompletions: List[CompletionCandidate] = toCandidates(camelMatches)
            def allowCompletion = (
              (memberCompletions.size == 1)
                || CompletionResult.camelMatch(r.name)(r.name.newName(StringOps.longestCommonPrefix(memberCompletions.map(_.defString))))
              )
            if (memberCompletions.isEmpty) NoCandidates
            else if (allowCompletion) (cursor - r.positionDelta, memberCompletions)
            else (cursor, CompletionCandidate("") :: memberCompletions)
          } else if (matching.nonEmpty && matching.forall(_.symNameDropLocal == r.name))
            NoCandidates // don't offer completion if the only option has been fully typed already
          else {
            // regular completion
            (cursor - r.positionDelta, toCandidates(matching))
          }
      }
      lastCommonPrefixCompletion =
        if (found != NoCandidates && buf.length >= found._1)
          Some(buf.substring(inputRange.start, found._1) + StringOps.longestCommonPrefix(found._2.map(_.defString)))
        else
          None
      found
    }

  }

}
