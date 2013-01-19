package scala.tools.reflect

import scala.tools.nsc.ast.parser.{Parsers => ScalaParser}
import scala.tools.nsc.ast.parser.Tokens._
import scala.compat.Platform.EOL
import scala.reflect.internal.util.{BatchSourceFile, SourceFile}

trait Parsers { self: Quasiquotes =>
  import global._

  class QParser extends {

    val global: self.global.type = self.global

  } with ScalaParser {

    def wrapCode(code: String): String =
      "object wrapper {" + EOL + code + EOL + "}"

    def unwrapTree(wrappedTree: Tree): Tree = {
      val PackageDef(_, List(ModuleDef(_, _, Template(_, _, _ :: parsed)))) = wrappedTree
      parsed match {
        case tree :: Nil => tree
        case stats :+ tree => Block(stats, tree)
      }
    }

    def parse(code: String, placeholders: Set[String]): Tree = {
      val file = new BatchSourceFile("<quasiquotes>", wrapCode(code))
      val tree = new QuasiquoteParser(file, placeholders).parse()
      unwrapTree(tree)
    }

    class QuasiquoteParser(source0: SourceFile, placeholders: Set[String]) extends SourceFileParser(source0) {
      // q"def foo($x)"
      override def allowTypelessParams = true

      // q"{ $x }"
      override def block(): Tree = makeBlock(blockStatSeq())
      private def makeBlock(stats: List[Tree]): Tree =
        if (stats.isEmpty) Literal(Constant())
        else if (!stats.last.isTerm) Block(stats, Literal(Constant()))
        else if (stats.length == 1) stats match {
          case Ident(TermName(name)) :: Nil if placeholders(name) => Block(stats.init, stats.last)
          case _ => stats.head
        } else Block(stats.init, stats.last)

      // q"foo match { $x }"
      override def caseClauses(): List[CaseDef] = {
        val cases = caseSeparated { atPos(in.offset)(treeBuilder.makeCaseDef(pattern(), guard(), caseBlock())) }
        if (cases.isEmpty) {
          if (in.token == IDENTIFIER && placeholders(in.name.toString)) ???
          else accept(CASE) // trigger error if there are no cases and noone gets spliced
        }
        cases
      }
    }
  }

  class TQParser extends QParser {

    override def wrapCode(code: String) = super.wrapCode("type T = " + code)

    override def unwrapTree(wrappedTree: Tree): Tree = {
      val TypeDef(_, _, _, rhs) = super.unwrapTree(wrappedTree)
      rhs
    }
  }
}