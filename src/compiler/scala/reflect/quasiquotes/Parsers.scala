package scala.reflect
package quasiquotes

import scala.tools.nsc.ast.parser.{Parsers => ScalaParser}
import scala.tools.nsc.ast.parser.Tokens._
import scala.reflect.internal.util.{BatchSourceFile, SourceFile, FreshNameCreator}

/** Builds upon the vanilla Scala parser and teams up together with Placeholders.scala to emulate holes.
 *  A principled solution to splicing into Scala syntax would be a parser that natively supports holes.
 *  Unfortunately, that's outside of our reach in Scala 2.11, so we have to emulate.
 */
trait Parsers { self: Quasiquotes =>
  import global.{Try => _, _}
  import build.implodePatDefs

  abstract class Parser extends {
    val global: self.global.type = self.global
  } with ScalaParser {
    def parse(code: String): Tree = {
      try {
        val file = new BatchSourceFile(nme.QUASIQUOTE_FILE, code)
        val parser = new QuasiquoteParser(file)
        parser.checkNoEscapingPlaceholders { parser.parseRule(entryPoint) }
      } catch {
        case mi: MalformedInput => c.abort(correspondingPosition(mi.offset), mi.msg)
      }
    }

    def correspondingPosition(offset: Int): Position = {
      val posMapList = posMap.toList
      def containsOffset(start: Int, end: Int) = start <= offset && offset < end
      def fallbackPosition = posMapList match {
        case (pos1, (start1, end1)) :: _   if start1 > offset => pos1
        case _ :+ ((pos2, (start2, end2))) if end2 <= offset  => pos2.withPoint(pos2.point + (end2 - start2))
      }
      posMapList.sliding(2).collect {
        case (pos1, (start1, end1)) :: _                        if containsOffset(start1, end1) => (pos1, offset - start1)
        case (pos1, (start1, end1)) :: (pos2, (start2, _)) :: _ if containsOffset(end1, start2) => (pos1, end1 - start1)
        case _ :: (pos2, (start2, end2)) :: _                   if containsOffset(start2, end2) => (pos2, offset - start2)
      }.map { case (pos, offset) =>
        pos.withPoint(pos.point + offset)
      }.toList.headOption.getOrElse(fallbackPosition)
    }

    override def token2string(token: Int): String = token match {
      case EOF => "end of quote"
      case _ => super.token2string(token)
    }

    def entryPoint: QuasiquoteParser => Tree

    class QuasiquoteParser(source0: SourceFile) extends SourceFileParser(source0) { parser =>
      def isHole: Boolean = isIdent && isHole(in.name)

      def isHole(name: Name): Boolean = holeMap.contains(name)

      override implicit lazy val fresh: FreshNameCreator = new FreshNameCreator(nme.QUASIQUOTE_PREFIX)

      // Do not check for tuple arity. The placeholders can support arbitrary tuple sizes.
      override def makeSafeTupleTerm(trees: List[Tree], offset: Offset): Tree = treeBuilder.makeTupleTerm(trees)
      override def makeSafeTupleType(trees: List[Tree], offset: Offset): Tree = treeBuilder.makeTupleType(trees)

      override val treeBuilder = new ParserTreeBuilder {
        override implicit def fresh: FreshNameCreator = parser.fresh

        // q"(..$xs)"
        override def makeTupleTerm(trees: List[Tree]): Tree = TuplePlaceholder(trees)

        // tq"(..$xs)"
        override def makeTupleType(trees: List[Tree]): Tree = TupleTypePlaceholder(trees)

        // q"{ $x }"
        override def makeBlock(stats: List[Tree]): Tree = method match {
          case nme.apply   =>
            stats match {
              // we don't want to eagerly flatten trees with placeholders as they
              // might have to be wrapped into a block depending on their value
              case (head @ Ident(name)) :: Nil if isHole(name) => Block(Nil, head)
              case _ => gen.mkBlock(stats, doFlatten = true)
            }
          case nme.unapply => gen.mkBlock(stats, doFlatten = false)
          case other       => global.abort("unreachable")
        }

        // tq"$a => $b"
        override def makeFunctionTypeTree(argtpes: List[Tree], restpe: Tree): Tree = FunctionTypePlaceholder(argtpes, restpe)

        // make q"val (x: T) = rhs" be equivalent to q"val x: T = rhs" for sake of bug compatibility (SI-8211)
        override def makePatDef(mods: Modifiers, pat: Tree, rhs: Tree) = pat match {
          case TuplePlaceholder(inParensPat :: Nil) => super.makePatDef(mods, inParensPat, rhs)
          case _ => super.makePatDef(mods, pat, rhs)
        }
      }
      import treeBuilder.{global => _, unit => _}

      // q"def foo($x)"
      override def param(owner: Name, implicitmod: Int, caseParam: Boolean): ValDef =
        if (isHole && lookingAhead { in.token == COMMA || in.token == RPAREN }) {
          ParamPlaceholder(implicitmod, ident())
        } else super.param(owner, implicitmod, caseParam)

      // q"($x) => ..." && q"class X { selfie => }
      override def convertToParam(tree: Tree): ValDef = tree match {
        case Ident(name) if isHole(name) => ParamPlaceholder(NoFlags, name)
        case _ => super.convertToParam(tree)
      }

      // q"foo match { case $x }"
      override def caseClause(): CaseDef =
        if (isHole && lookingAhead { in.token == CASE || in.token == RBRACE || in.token == SEMI }) {
          val c = CasePlaceholder(ident())
          while (in.token == SEMI) in.nextToken()
          c
        } else
          super.caseClause()

      override def caseBlock(): Tree = super.caseBlock() match {
        case Block(Nil, expr) => expr
        case other => other
      }

      override def isAnnotation: Boolean = super.isAnnotation || (isHole && lookingAhead { isAnnotation })

      override def isModifier: Boolean = super.isModifier || (isHole && lookingAhead { isModifier })

      override def isLocalModifier: Boolean = super.isLocalModifier || (isHole && lookingAhead { isLocalModifier })

      override def isTemplateIntro: Boolean = super.isTemplateIntro || (isHole && lookingAhead { isTemplateIntro })

      override def isDefIntro: Boolean = super.isDefIntro || (isHole && lookingAhead { isDefIntro })

      override def isDclIntro: Boolean = super.isDclIntro || (isHole && lookingAhead { isDclIntro })

      override def isStatSep(token: Int) = token == EOF || super.isStatSep(token)

      override def expectedMsg(token: Int): String =
        if (isHole) expectedMsgTemplate(token2string(token), "unquotee")
        else super.expectedMsg(token)

      // $mods def foo
      // $mods T
      override def readAnnots(annot: => Tree): List[Tree] = in.token match {
        case AT =>
          in.nextToken()
          annot :: readAnnots(annot)
        case _ if isHole && lookingAhead { isAnnotation || isModifier || isDefIntro || isIdent || isStatSep || in.token == LPAREN } =>
          val ann = ModsPlaceholder(in.name)
          in.nextToken()
          ann :: readAnnots(annot)
        case _ =>
          Nil
      }

      override def refineStat(): List[Tree] =
        if (isHole && !isDclIntro) {
          val result = RefineStatPlaceholder(in.name) :: Nil
          in.nextToken()
          result
        } else super.refineStat()

      override def ensureEarlyDef(tree: Tree) = tree match {
        case Ident(name: TermName) if isHole(name) => EarlyDefPlaceholder(name)
        case _ => super.ensureEarlyDef(tree)
      }

      override def isTypedParam(tree: Tree) = super.isTypedParam(tree) || (tree match {
        case Ident(name) if isHole(name) => true
        case _ => false
      })

      override def topStat = super.topStat.orElse {
        case _ if isHole =>
          val stats = PackageStatPlaceholder(in.name) :: Nil
          in.nextToken()
          stats
      }

      override def enumerator(isFirst: Boolean, allowNestedIf: Boolean = true) =
        if (isHole && lookingAhead { in.token == EOF || in.token == RPAREN || isStatSep }) {
          val res = ForEnumPlaceholder(in.name) :: Nil
          in.nextToken()
          res
        } else super.enumerator(isFirst, allowNestedIf)
    }
  }

  /** Wrapper around tree parsed in q"..." quote. Needed to support ..$ splicing on top-level. */
  object Q {
    def apply(tree: Tree): Block = Block(Nil, tree).updateAttachment(Q)
    def unapply(tree: Tree): Option[Tree] = tree match {
      case Block(Nil, contents) if tree.hasAttachment[Q.type] => Some(contents)
      case _ => None
    }
  }

  object TermParser extends Parser {
    def entryPoint = parser => Q(implodePatDefs(gen.mkTreeOrBlock(parser.templateOrTopStatSeq())))
  }

  object TypeParser extends Parser {
    def entryPoint = { parser =>
      if (parser.in.token == EOF)
        TypeTree()
      else
        parser.typ()
    }
  }

  object CaseParser extends Parser {
    def entryPoint = parser => implodePatDefs(parser.caseClause())
  }

  object PatternParser extends Parser {
    def entryPoint = { parser =>
      val pat = parser.noSeq.pattern()
      gen.patvarTransformer.transform(pat)
    }
  }

  object ForEnumeratorParser extends Parser {
    def entryPoint = { parser =>
      val enums = parser.enumerator(isFirst = false, allowNestedIf = false)
      assert(enums.length == 1)
      implodePatDefs(enums.head)
    }
  }

  object FreshName extends FreshNameExtractor(nme.QUASIQUOTE_PREFIX)
}
