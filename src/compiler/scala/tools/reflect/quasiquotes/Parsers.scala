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
    /** Wraps given code to obtain a desired parser mode.
     *  This way we can just re-use standard parser entry point.
     */
    def wrapCode(code: String): String =
      s"object wrapper { self => $EOL $code $EOL }"

    def unwrapTree(wrappedTree: Tree): Tree = {
      val PackageDef(_, List(ModuleDef(_, _, Template(_, _, _ :: parsed)))) = wrappedTree
      parsed match {
        case tree :: Nil => tree
        case stats :+ tree => Block(stats, tree)
      }
    }

    def parse(code: String): Tree = {
      try {
        val wrapped = wrapCode(code)
        debug(s"wrapped code\n=${wrapped}\n")
        val file = new BatchSourceFile(nme.QUASIQUOTE_FILE, wrapped)
        val tree = new QuasiquoteParser(file).parse()
        unwrapTree(tree)
      } catch {
        case mi: MalformedInput => c.abort(c.macroApplication.pos, s"syntax error: ${mi.msg}")
      }
    }

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

      def isHole = isIdent && holeMap.contains(in.name)

      override def isAnnotation: Boolean =  super.isAnnotation || (isHole && lookingAhead { isAnnotation })

      override def isModifier: Boolean = super.isModifier || (isHole && lookingAhead { isModifier })

      override def isLocalModifier: Boolean = super.isLocalModifier || (isHole && lookingAhead { isLocalModifier })

      override def isTemplateIntro: Boolean = super.isTemplateIntro || (isHole && lookingAhead { isTemplateIntro })

      override def isDclIntro: Boolean = super.isDclIntro || (isHole && lookingAhead { isDclIntro })

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

  object TermParser extends Parser

  object CaseParser extends Parser {
    override def wrapCode(code: String) = super.wrapCode("something match { case " + code + " }")

    override def unwrapTree(wrappedTree: Tree): Tree = {
      val Match(_, head :: tail) = super.unwrapTree(wrappedTree)
      if (tail.nonEmpty)
        c.abort(c.macroApplication.pos, "Can't parse more than one casedef, consider generating a match tree instead")
      head
    }
  }

  object PatternParser extends Parser {
    override def wrapCode(code: String) = super.wrapCode("something match { case " + code + " => }")

    override def unwrapTree(wrappedTree: Tree): Tree = {
      val Match(_, List(CaseDef(pat, _, _))) = super.unwrapTree(wrappedTree)
      pat
    }
  }

  object TypeParser extends Parser {
    override def wrapCode(code: String) = super.wrapCode("type T = " + code)

    override def unwrapTree(wrappedTree: Tree): Tree = {
      val TypeDef(_, _, _, rhs) = super.unwrapTree(wrappedTree)
      rhs
    }
  }
}