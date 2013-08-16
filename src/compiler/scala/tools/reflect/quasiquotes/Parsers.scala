package scala.tools.reflect
package quasiquotes

import scala.tools.nsc.ast.parser.{Parsers => ScalaParser}
import scala.tools.nsc.ast.parser.Tokens._
import scala.compat.Platform.EOL
import scala.reflect.internal.util.{BatchSourceFile, SourceFile}
import scala.collection.mutable.ListBuffer

/** Builds upon the vanilla Scala parser and teams up together with Placeholders.scala to emulate holes.
 *  A principled solution to splicing into Scala syntax would be a parser that natively supports holes.
 *  Unfortunately, that's outside of our reach in Scala 2.11, so we have to emulate.
 */
trait Parsers { self: Quasiquotes =>
  import global._

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

    class QuasiquoteParser(source0: SourceFile) extends SourceFileParser(source0) {
      override val treeBuilder = new ParserTreeBuilder {
        // q"(..$xs)"
        override def makeTupleTerm(trees: List[Tree], flattenUnary: Boolean): Tree =
          Apply(Ident(nme.QUASIQUOTE_TUPLE), trees)

        // tq"(..$xs)"
        override def makeTupleType(trees: List[Tree], flattenUnary: Boolean): Tree =
          AppliedTypeTree(Ident(tpnme.QUASIQUOTE_TUPLE), trees)

        // q"{ $x }"
        override def makeBlock(stats: List[Tree]): Tree = stats match {
          case (head @ Ident(name)) :: Nil if holeMap.contains(name) => Block(Nil, head)
          case _ => super.makeBlock(stats)
        }
      }
      import treeBuilder.{global => _, _}

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

      def isHole: Boolean = isIdent && holeMap.contains(in.name)

      override def isAnnotation: Boolean = super.isAnnotation || (isHole && lookingAhead { isAnnotation })

      override def isCaseDefStart: Boolean = super.isCaseDefStart || (in.token == EOF)

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
        case _ if isHole && lookingAhead { in.token == AT || isModifier || isDefIntro || isIdent} =>
          val ann = Apply(Select(New(Ident(tpnme.QUASIQUOTE_MODS)), nme.CONSTRUCTOR), List(Literal(Constant(in.name.toString))))
          in.nextToken()
          ann :: readAnnots(annot)
        case _ =>
          Nil
      }
    }
  }

  object TermParser extends Parser {
    def entryPoint = _.templateStats() match {
      case Nil => EmptyTree
      case tree :: Nil => tree
      case stats :+ tree => Block(stats, tree)
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
      parser.treeBuilder.patvarTransformer.transform(pat)
    }
  }
}