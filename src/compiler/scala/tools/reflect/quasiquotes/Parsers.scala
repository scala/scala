package scala.tools.reflect
package quasiquotes

import scala.tools.nsc.ast.parser.{Parsers => ScalaParser}
import scala.tools.nsc.ast.parser.Tokens._
import scala.compat.Platform.EOL
import scala.reflect.internal.util.{BatchSourceFile, SourceFile}
import scala.collection.mutable.ListBuffer

trait Parsers { self: Quasiquotes =>
  import global._

  abstract class Parser extends {
    val global: self.global.type = self.global
  } with ScalaParser {
    def wrapCode(code: String): String =
      s"object wrapper { $$wrapper$$self => $EOL $code $EOL }"

    def unwrapTree(wrappedTree: Tree): Tree = {
      val PackageDef(_, List(ModuleDef(_, _, Template(_, _, _ :: parsed)))) = wrappedTree
      parsed match {
        case tree :: Nil => tree
        case stats :+ tree => Block(stats, tree)
      }
    }

    def parse(code: String, holes: Set[String]): Tree = {
      try {
        val wrapped = wrapCode(code)
        debug(s"wrapped code\n=${wrapped}\n")
        val file = new BatchSourceFile(nme.QUASIQUOTE_FILE, wrapped)
        val tree = new QuasiquoteParser(file, holes).parse()
        unwrapTree(tree)
      } catch {
        case mi: MalformedInput => c.abort(c.macroApplication.pos, s"syntax error: ${mi.msg}")
      }
    }

    class QuasiquoteParser(source0: SourceFile, holes: Set[String]) extends SourceFileParser(source0) {
      override val treeBuilder = new ParserTreeBuilder {
        // q"(..$xs)"
        override def makeTupleTerm(trees: List[Tree], flattenUnary: Boolean): Tree =
          Apply(Ident(nme.QUASIQUOTE_TUPLE), trees)

        // tq"(..$xs)"
        override def makeTupleType(trees: List[Tree], flattenUnary: Boolean): Tree =
          AppliedTypeTree(Ident(tpnme.QUASIQUOTE_TUPLE), trees)

        // q"{ $x }"
        override def makeBlock(stats: List[Tree]): Tree = stats match {
          case Nil => Literal(Constant())
          case _ :+ last if !last.isTerm => Block(stats, Literal(Constant()))
          case (head @ Ident(TermName(name))) :: Nil if holes(name) => Block(Nil, head)
          case head :: Nil => head
          case first :+ last => Block(first, last)
        }
      }
      import treeBuilder.{global => _, _}

      // q"def foo($x)"
      override def allowTypelessParams = true

      // q"foo match { case $x }"
      override def caseClauses(): List[CaseDef] = {
        val cases = caseSeparated {
          val casedef =
            if (isPlaceholder && lookahead { in.token == CASE || in.token == RBRACE || in.token == SEMI }) {
              val c = makeCaseDef(Apply(Ident(nme.QUASIQUOTE_CASE), List(Ident(ident()))), EmptyTree, EmptyTree)
              while (in.token == SEMI) in.nextToken()
              c
            } else
              makeCaseDef(pattern(), guard(), caseBlock())
          atPos(in.offset)(casedef)
        }
        if (cases.isEmpty)  // trigger error if there are no cases
          accept(CASE)

        cases
      }

      private class PlainScannerData extends ScannerData {
        var ch: Char = _
        var charOffset: Int = 0
        var lineStartOffset: Int = 0
        var lastLineStartOffset: Int = 0
      }

      def lookahead[T](body: => T): T = {
        // back up scanner state
        val curr = new PlainScannerData().copyFrom(in)
        val prev = new PlainScannerData().copyFrom(in.prev)
        val next = new PlainScannerData().copyFrom(in.next)

        in.nextToken()
        val res = body

        // restore saves state
        in copyFrom curr
        in.prev copyFrom prev
        in.next copyFrom next

        res
      }

      def isPlaceholder = isIdent && holes.contains(in.name.toString)

      override def isAnnotation: Boolean =  super.isAnnotation || (isPlaceholder && lookahead { isAnnotation })

      override def isModifier: Boolean = super.isModifier || (isPlaceholder && lookahead { isModifier })

      override def isLocalModifier: Boolean = super.isLocalModifier || (isPlaceholder && lookahead { isLocalModifier })

      override def isTemplateIntro: Boolean = super.isTemplateIntro || (isPlaceholder && lookahead { isTemplateIntro })

      override def isDclIntro: Boolean = super.isDclIntro || (isPlaceholder && lookahead { isDclIntro })

      def modsPlaceholderAnnot(name: TermName): Tree =
        Apply(Select(New(Ident(tpnme.QUASIQUOTE_MODS)), nme.CONSTRUCTOR), List(Literal(Constant(name.toString))))

      // $mods def foo
      // $mods T
      def quasiquoteReadAnnots(annot: => Tree): List[Tree] = {
        val annots = new ListBuffer[Tree]
        var break = false

        while (!break) {
          if (in.token == AT) {
            in.nextToken()
            annots += annot
          } else if (isPlaceholder && lookahead { in.token == AT || isModifier || isDefIntro || isIdent}) {
            annots += modsPlaceholderAnnot(in.name)
            in.nextToken()
          } else {
            break = true
          }
        }
        annots.toList
      }

      override def annotations(skipNewLines: Boolean): List[Tree] = quasiquoteReadAnnots {
        val t = annotationExpr()
        if (skipNewLines) newLineOpt()
        t
      }

      override def constructorAnnotations(): List[Tree] = quasiquoteReadAnnots {
        atPos(in.offset)(New(exprSimpleType(), List(argumentExprs())))
      }
    }
  }

  object TermParser extends Parser

  object CaseParser extends Parser {
    override def wrapCode(code: String) = super.wrapCode("something match { " + code + " }")

    override def unwrapTree(wrappedTree: Tree): Tree = {
      val Match(_, head :: tail) = super.unwrapTree(wrappedTree)
      if (tail.nonEmpty)
        c.abort(c.macroApplication.pos, "can't parse more than one casedef, generate match tree instead")
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