/* NSC -- new Scala compiler
 * Copyright 2007-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package doc

import scala.tools.nsc.ast.parser.{ SyntaxAnalyzer, BracePatch }
import typechecker.Analyzer
import scala.reflect.internal.Chars._
import scala.reflect.internal.util.{ BatchSourceFile, Position }
import scala.tools.nsc.doc.base.{ CommentFactoryBase, MemberLookupBase, LinkTo, LinkToExternal }

trait ScaladocAnalyzer extends Analyzer {
  val global : Global // generally, a ScaladocGlobal
  import global._

  override def newTyper(context: Context): ScaladocTyper = new Typer(context) with ScaladocTyper

  trait ScaladocTyper extends Typer {
    private def unit = context.unit

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
              reporter.warning(useCase.pos, err.errMsg)
            case _ =>
          }
          for (useCaseSym <- useCase.defined) {
            if (sym.name != useCaseSym.name)
              reporter.warning(useCase.pos, "@usecase " + useCaseSym.name.decode + " does not match commented symbol: " + sym.name.decode)
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

      if (settings.debug)
        useCase.defined foreach (sym => println("defined use cases: %s:%s".format(sym, sym.tpe)))

      useCase.defined
    }
  }
}

abstract class ScaladocSyntaxAnalyzer[G <: Global](val global: G) extends SyntaxAnalyzer {
  import global._

  class ScaladocJavaUnitParser(unit: CompilationUnit) extends {
    override val in = new ScaladocJavaUnitScanner(unit)
  } with JavaUnitParser(unit) { }

  class ScaladocJavaUnitScanner(unit: CompilationUnit) extends JavaUnitScanner(unit) {
    /** buffer for the documentation comment
     */
    var docBuffer: StringBuilder = null

    /** add the given character to the documentation buffer
     */
    protected def putDocChar(c: Char) {
      if (docBuffer ne null) docBuffer.append(c)
    }

    override protected def skipComment(): Boolean = {
      if (in.ch == '/') {
        do {
          in.next
        } while ((in.ch != CR) && (in.ch != LF) && (in.ch != SU))
        true
      } else if (in.ch == '*') {
        docBuffer = null
        in.next
        val scaladoc = ("/**", "*/")
        if (in.ch == '*')
          docBuffer = new StringBuilder(scaladoc._1)
        do {
          do {
            if (in.ch != '*' && in.ch != SU) {
              in.next; putDocChar(in.ch)
            }
          } while (in.ch != '*' && in.ch != SU)
          while (in.ch == '*') {
            in.next; putDocChar(in.ch)
          }
        } while (in.ch != '/' && in.ch != SU)
        if (in.ch == '/') in.next
        else incompleteInputError("unclosed comment")
        true
      } else {
        false
      }
    }
  }

  class ScaladocUnitScanner(unit0: CompilationUnit, patches0: List[BracePatch]) extends UnitScanner(unit0, patches0) {

    private var docBuffer: StringBuilder = null        // buffer for comments (non-null while scanning)
    private var inDocComment             = false       // if buffer contains double-star doc comment
    private var lastDoc: DocComment      = null        // last comment if it was double-star doc

    private object unmooredParser extends {                // minimalist comment parser
      val global: Global = ScaladocSyntaxAnalyzer.this.global
    }
    with CommentFactoryBase with MemberLookupBase {
      import global.{ settings, Symbol }
      def parseComment(comment: DocComment) = {
        val nowarnings = settings.nowarn.value
        settings.nowarn.value = true
        try parseAtSymbol(comment.raw, comment.raw, comment.pos)
        finally settings.nowarn.value = nowarnings
      }

      override def internalLink(sym: Symbol, site: Symbol): Option[LinkTo] = None
      override def chooseLink(links: List[LinkTo]): LinkTo = links.headOption.orNull
      override def toString(link: LinkTo): String = "No link"
      override def findExternalLink(sym: Symbol, name: String): Option[LinkToExternal] = None
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
      val doc = flushDoc
      // tags that make a local double-star comment look unclean, as though it were API
      def unclean(comment: Comment): Boolean = {
        import comment._
        authors.nonEmpty || result.nonEmpty || throws.nonEmpty || valueParams.nonEmpty ||
        typeParams.nonEmpty || version.nonEmpty || since.nonEmpty
      }
      def isDirty = unclean(unmooredParser parseComment doc)
      if ((doc ne null) && (settings.warnDocDetached || isDirty))
        reporter.warning(doc.pos, "discarding unmoored doc comment")
    }

    override def flushDoc(): DocComment = (try lastDoc finally lastDoc = null)

    override protected def putCommentChar() {
      if (inDocComment)
        docBuffer append ch

      nextChar()
    }
    override def skipDocComment(): Unit = {
      inDocComment = true
      docBuffer = new StringBuilder("/**")
      super.skipDocComment()
    }
    override def skipBlockComment(): Unit = {
      inDocComment = false // ??? this means docBuffer won't receive contents of this comment???
      docBuffer = new StringBuilder("/*")
      super.skipBlockComment()
    }
    override def skipComment(): Boolean = {
      // emit a block comment; if it's double-star, make Doc at this pos
      def foundStarComment(start: Int, end: Int) = try {
        val str = docBuffer.toString
        val pos = Position.range(unit.source, start, start, end)
        if (inDocComment) {
          signalParsedDocComment(str, pos)
          lastDoc = DocComment(str, pos)
        }
        true
      } finally {
        docBuffer    = null
        inDocComment = false
      }
      super.skipComment() && ((docBuffer eq null) || foundStarComment(offset, charOffset - 2))
    }
  }
  class ScaladocUnitParser(unit: CompilationUnit, patches: List[BracePatch]) extends UnitParser(unit, patches) {
    override def newScanner() = new ScaladocUnitScanner(unit, patches)
    override def withPatches(patches: List[BracePatch]) = new ScaladocUnitParser(unit, patches)

    override def joinComment(trees: => List[Tree]): List[Tree] = {
      val doc = in.flushDoc
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
}
