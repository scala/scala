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

package scala.tools.nsc
package doc

import scala.tools.nsc.ast.parser.{BracePatch, SyntaxAnalyzer}
import typechecker.Analyzer
import scala.reflect.internal.util.{BatchSourceFile, Position}
import scala.tools.nsc.Reporting.WarningCategory
import scala.tools.nsc.doc.base.{CommentFactoryBase, LinkTo, MemberLookupBase}

trait ScaladocAnalyzer extends Analyzer {
  val global : Global // generally, a ScaladocGlobal
  import global._

  override def newTyper(context: Context): ScaladocTyper = new Typer(context) with ScaladocTyper

  trait ScaladocTyper extends Typer {

    override def canAdaptConstantTypeToLiteral = false

    override protected def macroImplementationNotFoundMessage(name: Name): String = (
        super.macroImplementationNotFoundMessage(name)
      + "\nWhen generating scaladocs for multiple projects at once, consider using -Ymacro-no-expand to disable macro expansions altogether."
    )

    override def typedDocDef(docDef: DocDef, mode: Mode, pt: Type): Tree = {
      val sym = docDef.symbol

      if ((sym ne null) && (sym ne NoSymbol)) {
        val comment = docDef.comment
        docComments(sym) = comment
        comment.defineVariables(sym)
        val typer1 = newTyper(context.makeNewScope(docDef, context.owner))
        for (useCase <- comment.useCases) {
          typer1.silent(_.asInstanceOf[ScaladocTyper].defineUseCases(useCase)) match {
            case SilentTypeError(err) =>
              context.warning(useCase.pos, err.errMsg, WarningCategory.Scaladoc)
            case _ =>
          }
          for (useCaseSym <- useCase.defined) {
            if (sym.getterName != useCaseSym.getterName)
              context.warning(useCase.pos, "@usecase " + useCaseSym.name.decode + " does not match commented symbol: " + sym.name.decode, WarningCategory.Scaladoc)
          }
        }
      }

      super.typedDocDef(docDef, mode, pt)
    }

    def defineUseCases(useCase: UseCase): List[Symbol] = {
      def stringParser(str: String): syntaxAnalyzer.Parser = {
        val file = new BatchSourceFile(context.unit.source.file, str) {
          override def positionInUltimateSource(pos: Position) = {
            pos withSource context.unit.source withShift useCase.pos.start
          }
        }
        newUnitParser(new CompilationUnit(file))
      }

      val trees = stringParser(useCase.body+";").nonLocalDefOrDcl
      val enclClass = context.enclClass.owner

      def defineAlias(name: Name) = (
        if (context.scope.lookup(name) == NoSymbol) {
          lookupVariable(name.toString.substring(1), enclClass) foreach { repl =>
            silent(_.typedTypeConstructor(stringParser(repl).typ())) map { tpt =>
              val alias = enclClass.newAliasType(name.toTypeName, useCase.pos)
              val tparams = cloneSymbolsAtOwner(tpt.tpe.typeSymbol.typeParams, alias)
              val newInfo = genPolyType(tparams, appliedType(tpt.tpe, tparams map (_.tpe)))
              alias setInfo newInfo
              context.scope.enter(alias)
            }
          }
        }
      )

      for (tree <- trees; t <- tree)
        t match {
          case Ident(name) if name startsWith '$' => defineAlias(name)
          case _ =>
        }

      useCase.aliases = context.scope.toList
      namer.enterSyms(trees)
      typedStats(trees, NoSymbol)
      useCase.defined = context.scope.toList filterNot (useCase.aliases contains _)

      if (settings.isDebug)
        useCase.defined foreach (sym => println("defined use cases: %s:%s".format(sym, sym.tpe)))

      useCase.defined
    }
  }
}

abstract class ScaladocSyntaxAnalyzer[G <: Global](val global: G) extends SyntaxAnalyzer {
  import global._

  trait ScaladocScanner extends DocScanner {
    // When `docBuffer == null`, we're not in a doc comment.
    private var docBuffer: StringBuilder = null

    override protected def beginDocComment(prefix: String): Unit =
      if (docBuffer == null) docBuffer = new StringBuilder(prefix)

    protected def ch: Char
    override protected def processCommentChar(): Unit =
      if (docBuffer != null) docBuffer append ch

    protected def docPosition: Position
    override protected def finishDocComment(): Unit =
      if (docBuffer != null) {
        registerDocComment(docBuffer.toString, docPosition)
        docBuffer = null
      }
  }

  class ScaladocUnitScanner(unit0: CompilationUnit, patches0: List[BracePatch]) extends UnitScanner(unit0, patches0) with ScaladocScanner {
    private object unmooredParser extends {                // minimalist comment parser
      val global: Global = ScaladocSyntaxAnalyzer.this.global
    }
    with CommentFactoryBase with MemberLookupBase {
      import global.{ settings, Symbol }
      def parseComment(comment: DocComment) = {
        val nowarnings = settings.nowarn.value
        val maxwarns   = settings.maxwarns.value
        if (!nowarnings) {
          settings.nowarn.value = true
        }
        try parseAtSymbol(comment.raw, comment.raw, comment.pos)
        finally
          if (!nowarnings) {
            settings.nowarn.value   = false
            settings.maxwarns.value = maxwarns
          }
      }

      override def internalLink(sym: Symbol, site: Symbol): Option[LinkTo] = None
      override def chooseLink(links: List[LinkTo]): LinkTo = links.headOption.orNull
      override def toString(link: LinkTo): String = "No link"
      override def findExternalLink(sym: Symbol, name: String): Option[LinkTo] = None
      override def warnNoLink: Boolean = false
    }

    /**
     * Warn when discarding buffered doc at the end of a block.
     * This mechanism doesn't warn about arbitrary unmoored doc.
     * Also warn under -Xlint, but otherwise only warn in the presence of suspicious
     * tags that appear to be documenting API.  Warnings are suppressed while parsing
     * the local comment so that comments of the form `[at] Martin` will not trigger a warning.
     * By omission, tags for `see`, `todo`, `note` and `example` are ignored.
     */
    override def discardDocBuffer() = {
      import scala.tools.nsc.doc.base.comment.Comment
      val doc = flushDoc()
      // tags that make a local double-star comment look unclean, as though it were API
      def unclean(comment: Comment): Boolean = {
        import comment._
        authors.nonEmpty || result.nonEmpty || throws.nonEmpty || valueParams.nonEmpty ||
        typeParams.nonEmpty || version.nonEmpty || since.nonEmpty
      }
      def isDirty = unclean(unmooredParser parseComment doc)
      if ((doc ne null) && (settings.warnDocDetached || isDirty))
        runReporting.warning(doc.pos, "discarding unmoored doc comment", WarningCategory.LintDocDetached, site = "")
    }

    protected def docPosition: Position = Position.range(unit.source, offset, offset, charOffset - 2)
  }
  class ScaladocUnitParser(unit: CompilationUnit, patches: List[BracePatch]) extends UnitParser(unit, patches) {
    override def newScanner() = new ScaladocUnitScanner(unit, patches)
    override def withPatches(patches: List[BracePatch]) = new ScaladocUnitParser(unit, patches)

    override def joinComment(trees: => List[Tree]): List[Tree] = {
      val doc = in.flushDoc()
      if ((doc ne null) && doc.raw.length > 0) {
        log(s"joinComment(doc=$doc)")
        val joined = trees map {
          t =>
            DocDef(doc, t) setPos {
              if (t.pos.isDefined) {
                val pos = doc.pos.withEnd(t.pos.end)
                // always make the position transparent
                pos.makeTransparent
              } else {
                t.pos
              }
            }
        }
        joined.find(_.pos.isOpaqueRange) foreach {
          main =>
            val mains = List(main)
            joined foreach { t => if (t ne main) ensureNonOverlapping(t, mains) }
        }
        joined
      }
      else trees
    }
  }

  class ScaladocJavaUnitScanner(unit: CompilationUnit) extends JavaUnitScanner(unit) with ScaladocScanner {
    private var docStart: Int = 0

    override protected def beginDocComment(prefix: String): Unit = {
      super.beginDocComment(prefix)
      docStart = currentPos.start
    }

    protected def ch = in.ch

    override protected def docPosition = Position.range(unit.source, docStart, docStart, in.cpos)
  }

  class ScaladocJavaUnitParser(unit: CompilationUnit) extends {
    override val in = new ScaladocJavaUnitScanner(unit)
  } with JavaUnitParser(unit) {

    override def joinComment(trees: => List[Tree]): List[Tree] = {
      val doc = in.flushDoc()

      if ((doc ne null) && doc.raw.length > 0) {
        log(s"joinComment(doc=$doc)")
        val joined = trees map { t =>
          DocDef(doc, t) setPos {
            if (t.pos.isDefined) {
              val pos = doc.pos.withEnd(t.pos.end)
              pos.makeTransparent
            } else {
              t.pos
            }
          }
        }
        joined.find(_.pos.isOpaqueRange) foreach { main =>
          val mains = List(main)
          joined foreach { t => if (t ne main) ensureNonOverlapping(t, mains) }
        }
        joined
      } else {
        trees
      }
    }
  }
}
