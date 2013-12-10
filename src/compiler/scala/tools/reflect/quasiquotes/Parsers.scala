package scala.tools.reflect
package quasiquotes

import scala.tools.nsc.ast.parser.{Parsers => ScalaParser}
import scala.tools.nsc.ast.parser.Tokens._
import scala.compat.Platform.EOL
import scala.reflect.internal.util.{BatchSourceFile, SourceFile, FreshNameCreator}
import scala.collection.mutable.ListBuffer
import scala.util.Try

/** Builds upon the vanilla Scala parser and teams up together with Placeholders.scala to emulate holes.
 *  A principled solution to splicing into Scala syntax would be a parser that natively supports holes.
 *  Unfortunately, that's outside of our reach in Scala 2.11, so we have to emulate.
 */
trait Parsers { self: Quasiquotes =>
  import global.{Try => _, _}

  abstract class Parser extends {
    val global: self.global.type = self.global
  } with ScalaParser {
    def parse(code: String): Tree = {
      try {
        val file = new BatchSourceFile(nme.QUASIQUOTE_FILE, code)
        new QuasiquoteParser(file).parseRule(entryPoint)
      } catch {
        case mi: MalformedInput => c.abort(correspondingPosition(mi.offset), mi.msg)
      }
    }

    def correspondingPosition(offset: Int): Position = {
      val posMapList = posMap.toList
      def containsOffset(start: Int, end: Int) = start <= offset && offset <= end
      def fallbackPosition = posMapList match {
        case (pos1, (start1, end1)) :: _   if start1 > offset => pos1
        case _ :+ ((pos2, (start2, end2))) if offset > end2   => pos2.withPoint(pos2.point + (end2 - start2))
      }
      posMapList.sliding(2).collect {
        case (pos1, (start1, end1)) :: _                if containsOffset(start1, end1) => (pos1, offset - start1)
        case (pos1, (_, end1)) :: (_, (start2, _)) :: _ if containsOffset(end1, start2) => (pos1, end1)
        case _ :: (pos2, (start2, end2)) :: _           if containsOffset(start2, end2) => (pos2, offset - start2)
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

      override implicit def fresh: FreshNameCreator = new FreshNameCreator(nme.QUASIQUOTE_PREFIX)

      override val treeBuilder = new ParserTreeBuilder {
        override implicit def fresh: FreshNameCreator = parser.fresh

        // q"(..$xs)"
        override def makeTupleTerm(trees: List[Tree]): Tree =
          Apply(Ident(nme.QUASIQUOTE_TUPLE), trees)

        // tq"(..$xs)"
        override def makeTupleType(trees: List[Tree]): Tree =
          AppliedTypeTree(Ident(tpnme.QUASIQUOTE_TUPLE), trees)

        // q"{ $x }"
        override def makeBlock(stats: List[Tree]): Tree = stats match {
          case (head @ Ident(name)) :: Nil if isHole(name) => Block(Nil, head)
          case _ => super.makeBlock(stats)
        }

        // tq"$a => $b"
        override def makeFunctionTypeTree(argtpes: List[Tree], restpe: Tree): Tree =
          AppliedTypeTree(Ident(tpnme.QUASIQUOTE_FUNCTION), argtpes :+ restpe)
      }
      import treeBuilder.{global => _, unit => _, _}

      // q"def foo($x)"
      override def allowTypelessParams = true

      // q"foo match { case $x }"
      override def caseClause(): CaseDef =
        if (isHole && lookingAhead { in.token == CASE || in.token == RBRACE || in.token == SEMI }) {
          val c = makeCaseDef(Apply(Ident(nme.QUASIQUOTE_CASE), List(Ident(ident()))), EmptyTree, EmptyTree)
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

      override def isDclIntro: Boolean = super.isDclIntro || (isHole && lookingAhead { isDclIntro })

      override def isStatSep(token: Int) = token == EOF || super.isStatSep(token)

      override def expectedMsg(token: Int): String =
        if (isHole) expectedMsgTemplate(token2string(token), "splicee")
        else super.expectedMsg(token)

      // $mods def foo
      // $mods T
      override def readAnnots(annot: => Tree): List[Tree] = in.token match {
        case AT =>
          in.nextToken()
          annot :: readAnnots(annot)
        case _ if isHole && lookingAhead { isAnnotation || isModifier || isDefIntro || isIdent || isStatSep || in.token == LPAREN } =>
          val ann = Apply(Select(New(Ident(tpnme.QUASIQUOTE_MODS)), nme.CONSTRUCTOR), List(Literal(Constant(in.name.toString))))
          in.nextToken()
          ann :: readAnnots(annot)
        case _ =>
          Nil
      }

      override def refineStat(): List[Tree] =
        if (isHole && !isDclIntro) {
          val result = ValDef(NoMods, in.name, Ident(tpnme.QUASIQUOTE_REFINE_STAT), EmptyTree) :: Nil
          in.nextToken()
          result
        } else super.refineStat()

      override def ensureEarlyDef(tree: Tree) = tree match {
        case Ident(name: TermName) if isHole(name) => ValDef(NoMods | Flag.PRESUPER, name, Ident(tpnme.QUASIQUOTE_EARLY_DEF), EmptyTree)
        case _ => super.ensureEarlyDef(tree)
      }

      override def isTypedParam(tree: Tree) = super.isTypedParam(tree) || (tree match {
        case Ident(name) if isHole(name) => true
        case _ => false
      })

      override def topStat = super.topStat.orElse {
        case _ if isHole =>
          val stats = ValDef(NoMods, in.name, Ident(tpnme.QUASIQUOTE_PACKAGE_STAT), EmptyTree) :: Nil
          in.nextToken()
          stats
      }

      override def enumerator(isFirst: Boolean, allowNestedIf: Boolean = true) =
        if (isHole && lookingAhead { in.token == EOF || in.token == RPAREN || isStatSep }) {
          val res = build.SyntacticValFrom(Bind(in.name, Ident(nme.WILDCARD)), Ident(nme.QUASIQUOTE_FOR_ENUM)) :: Nil
          in.nextToken()
          res
        } else super.enumerator(isFirst, allowNestedIf)
    }
  }

  object TermParser extends Parser {
    def entryPoint = { parser =>
      parser.templateOrTopStatSeq() match {
        case head :: Nil => Block(Nil, head)
        case lst => gen.mkTreeOrBlock(lst)
      }
    }
  }

  object TypeParser extends Parser {
    def entryPoint = _.typ()
  }

  object CaseParser extends Parser {
    def entryPoint = _.caseClause()
  }

  object PatternParser extends Parser {
    def entryPoint = { parser =>
      val pat = parser.noSeq.pattern1()
      gen.patvarTransformer.transform(pat)
    }
  }

  object ForEnumeratorParser extends Parser {
    def entryPoint = { parser =>
      val enums = parser.enumerator(isFirst = false, allowNestedIf = false)
      assert(enums.length == 1)
      enums.head
    }
  }

  object FreshName extends FreshNameExtractor(nme.QUASIQUOTE_PREFIX)
}