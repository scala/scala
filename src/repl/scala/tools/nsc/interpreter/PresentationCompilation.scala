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

import scala.collection.mutable
import scala.reflect.internal.util.{Position, RangePosition}
import scala.tools.nsc.ast.parser.Tokens
import scala.tools.nsc.backend.JavaPlatform
import scala.tools.nsc.util.ClassPath
import scala.tools.nsc.{Settings, interactive}
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.classpath._
import scala.tools.nsc.interpreter.Results.{Error, Result}

trait PresentationCompilation { self: IMain =>

  private final val Cursor = IMain.DummyCursorFragment

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
      def cursorIsInKeyword(): Boolean = {
        val scanner = pc.newUnitParser(buf).newScanner()
        scanner.init()
        while (scanner.token != Tokens.EOF) {
          val token    = scanner.token
          val o        = scanner.offset
          scanner.nextToken()
          if ((o to scanner.lastOffset).contains(cursor)) {
            return (!Tokens.isIdentifier(token) && pc.syntaxAnalyzer.token2name.contains(token))
          }
        }
        false
      }
      // Support completion of "def format = 42; for<TAB>" by replacing the keyword with foo_CURSOR_ before
      // typechecking. Only do this when needed to be able ot correctly return the type of `foo.bar<CURSOR>`
      // where `bar` is the complete name of a member.
      val line1 = if (!cursorIsInKeyword()) buf else buf.patch(cursor, Cursor, 0)

      val parserSource = pc.newSourceFile(line1)
      val trees = pc.newUnitParser(new pc.CompilationUnit(parserSource)).parseStats() match {
        case Nil => List(pc.EmptyTree)
        case xs => xs
      }
      val importer = global.mkImporter(pc)
      //println(s"pc: [[$line1]], <<${trees.size}>>")
      val request = new Request(line1, trees map (t => importer.importTree(t)), parserSource, generousImports = true, storeResultInVal = false)
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

    def typeString(tree: compiler.Tree): String = {
      tree.tpe match {
        case null | compiler.NoType | compiler.ErrorType => ""
        case tp if compiler.nme.isReplWrapperName(tp.typeSymbol.name) => ""
        case tp => compiler.exitingTyper(tp.toString)
      }
    }

    def treeString(tree: compiler.Tree): String =
      compiler.showCode(tree)

    override def print = {
      val tree = treeAt(inputRange)
      import compiler._
      object makeCodePrinterPrintInferredTypes extends Transformer {
        private def printableTypeTree(tp: Type): TypeTree = {
          val tree = TypeTree(tp)
          tree.wasEmpty = false
          tree
        }
        override def transform(tree: Tree): Tree = super.transform(tree) match {
          case ValDef(mods, name, tt @ build.SyntacticEmptyTypeTree(), rhs) =>
            if (tree.symbol != null && tree.symbol != NoSymbol && nme.isReplWrapperName(tree.symbol.owner.name)) {
              treeCopy.ValDef(tree, mods &~ (Flag.PRIVATE | Flag.LOCAL), name.dropLocal, printableTypeTree(tt.tpe), rhs)
            } else {
              treeCopy.ValDef(tree, mods, name, printableTypeTree(tt.tpe), rhs)
            }
          case DefDef(mods, name, tparams, vparamss, tt @ build.SyntacticEmptyTypeTree(), rhs) =>
            treeCopy.DefDef(tree, mods, name, tparams, vparamss, printableTypeTree(tt.tpe), rhs)
          case t => t
        }

      }
      val tree1    = makeCodePrinterPrintInferredTypes.transform(tree)
      val tpString = typeString(tree1) match {
        case "" => ""
        case s => " // : "  + s
      }
      treeString(tree1) + tpString
    }


    override def typeAt(start: Int, end: Int) =
      typeString(typedTreeAt(start, end))

    val NoCandidates = (-1, Nil)
    type Candidates = (Int, List[CompletionCandidate])

    override def completionCandidates(filter: Boolean, tabCount: Int): Candidates = {
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
      def defStringCandidates(matching: List[Member], isNew: Boolean): List[CompletionCandidate] = {
        val seen = new mutable.HashSet[Symbol]()
        val ccs = for {
          member <- matching
          if seen.add(member.sym)
          sym <- if (member.sym.isClass && isNew) member.sym.info.decl(nme.CONSTRUCTOR).alternatives else member.sym.alternatives
          sugared = sym.sugaredSymbolOrSelf
        } yield {
          CompletionCandidate(
            name = member.symNameDropLocal.decoded,
            arity = memberArity(member),
            isDeprecated = isMemberDeprecated(member),
            isUniversal = isMemberUniversal(member),
            declString = () => {
              if (sym.isPackageObjectOrClass) ""
              else {
                val tp              = member.prefix memberType sym
                val desc            = Seq(if (isMemberDeprecated(member)) "(deprecated)" else "", if (isMemberUniversal(member)) "(universal)" else "")
                val methodOtherDesc = if (!desc.exists(_ != "")) "" else " " + desc.filter(_ != "").mkString(" ")
                sugared.defStringSeenAs(tp) + methodOtherDesc
              }
            },
            alias = member.aliasInfo.fold[Option[String]](None)(s => Some(s.sym.nameString))
            )
        }
        ccs
      }
      val found = this.completionsAt(cursor) match {
        case NoResults => NoCandidates
        case r =>
          def shouldHide(m: Member): Boolean =
            filter && tabCount == 0 && (isMemberDeprecated(m) || isMemberUniversal(m))
          val matching = r.matchingResults(nameMatcher = if (filter) {entered => candidate => candidate.startsWith(entered)} else _ => _ => true).filterNot(shouldHide)
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
          val candidates = defStringCandidates(matching, isNew)
          val pos = cursor - r.positionDelta
          (pos, candidates.sortBy(_.name))
      }
      found
    }

  }

}
