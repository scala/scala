/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package interpreter

import util.{ BatchSourceFile, Indenter }
import scala.tools.nsc.ast.parser.Tokens._
import java.lang.Integer.toOctalString

/** This began as an attempt at a completely minimal
 *  pretty printer for a token stream, but as it turns out
 *  it's "minimal, pretty, scala: pick any two." So
 *  now it's an unattractive hybrid between minimalism
 *  and other things.  Still, it's a big improvement on the
 *  way I was printing source in the repl, so in it goes.
 *
 *  @author   Paul Phillips
 */
abstract class ReplTokens {
  val global: Global

  import global._
  import syntaxAnalyzer.{ UnitScanner, token2name }

  // Mostly, this means print <NL> so we can see where
  // semicolon inference took place.
  private var rawTokens: Boolean = false
  def withRawTokens[T](body: => T): T = {
    rawTokens = true
    try body
    finally rawTokens = false
  }
  // There's the seed of a good idea in here, but you wouldn't
  // know it from the current implementation.  The objects are
  // trying to depict what feelings of coziness a given token
  // might have toward its immediate neighbors.  But it lacks
  // sufficient granularity and a good resolution mechanism.
  sealed abstract class Cozy(l: => Boolean, r: => Boolean) {
    def left = l
    def right = r
  }
  object Cozy {
    def unapply(x: Cozy) = Some((x.left, x.right))
  }
  case object  |--*--|  extends Cozy(false, false)
  case object    <*--|  extends Cozy(true, false)
  case object  |--*>    extends Cozy(false, true)
  case object    <*>    extends Cozy(true, true)

  @annotation.switch def escapedChar(ch: Char): String = ch match {
    case '\b' => "\\b"
    case '\t' => "\\t"
    case '\n' => "\\n"
    case '\f' => "\\f"
    case '\r' => "\\r"
    case '"'  => "\\\""
    case '\'' => "\\\'"
    case '\\' => "\\\\"
    case _    => String.valueOf(ch)
  }
  def escape(text: String): String = {
    text map { ch =>
      if (ch.isControl) "\\0" + toOctalString(ch)
      else escapedChar(ch)
    } mkString ""
  }
  private class Arrow(code: Int) {
    def ->(str: String): (Int, ReplToken)     = (code, Token(code)(str))
    def ->(tok: ReplToken): (Int, ReplToken)  = (code, tok)
  }
  private val symbolTokenMap = {
    implicit def liftToken(code: Int): Arrow = new Arrow(code)

    Map[Int, ReplToken](
      AT         -> At,
      CASECLASS  -> "case class",
      CASEOBJECT -> "case object",
      COLON      -> Colon,
      COMMA      -> Comma,
      DOT        -> Dot,
      EOF        -> Eof,
      ERROR      -> "<error>",
      FALSE      -> False,
      IMPORT     -> Import,
      LBRACE     -> LBrace,
      LBRACKET   -> LBracket,
      LPAREN     -> LParen,
      NEWLINE    -> Newline,
      NEWLINES   -> Newlines,
      NULL       -> Null,
      RBRACE     -> RBrace,
      RBRACKET   -> RBracket,
      RPAREN     -> RParen,
      SEMI       -> Semi,
      SUBTYPE    -> Subtype,
      SUPERTYPE  -> Supertype,
      TRUE       -> True,
      VIEWBOUND  -> ViewBound,
      XMLSTART   -> "<xmlstart>"
    )
  }
  def isAlphaId(t: ReplToken) = t match {
    case Id(name) => name forall (ch => ch.isDigit || ch.isLetter || ch == '_')
    case _ => false
  }
  def isOperatorId(t: ReplToken) = t match {
    case Id(name) => !isAlphaId(t)
    case _        => false
  }

  sealed abstract class ReplToken(val tokenString: String, val cozy: Cozy) {
    def this(str: String) = this(str, |--*--| )

    def insistsOnSpace = false
    def cozyRight(other: ReplToken) = (cozy.right || other.cozy.left)
    def cozyLeft(other: ReplToken)  = (cozy.left || other.cozy.right)

    final def <--?-->(other: ReplToken) = {
      !(insistsOnSpace || other.insistsOnSpace) && (
        (this cozyRight other) ||
        (other cozyLeft this)
      )
    }

    // to show invisibles
    def rawString = tokenString
    override def toString = (
      if (rawTokens) rawString
      else tokenString
    )
  }
  trait InsistCozyRight extends ReplToken {
    final override def cozyRight(other: ReplToken) = true
  }
  trait InsistCozyLeft extends ReplToken {
    final override def cozyLeft(other: ReplToken) = true
  }
  trait InsistCozy extends InsistCozyLeft with InsistCozyRight { }
  trait InsistSpaced extends ReplToken {
    final override def insistsOnSpace = true
  }
  trait CozyWithLetters extends ReplToken {
    override def cozyRight(other: ReplToken) = isAlphaId(other) || super.cozyRight(other)
    override def cozyLeft(other: ReplToken) = isAlphaId(other) || super.cozyLeft(other)
  }
  trait Brackets extends ReplToken {
    private def isCozyToken(t: ReplToken)    = t == LBracket || t == RBracket || isAlphaId(t)
    override def cozyRight(other: ReplToken) = isCozyToken(other) || super.cozyRight(other)
    override def cozyLeft(other: ReplToken)  = isCozyToken(other) || super.cozyLeft(other)
  }

  case class Token(value: Int)(str: String) extends ReplToken(str) { }
  case class Id(name: String) extends ReplToken(name) { }
  case class Lit[T](value: T) extends ReplToken(value match {
    case s: String  => "\"" + s + "\""
    case _          => "" + value
  })
  case object At extends ReplToken("@") with InsistCozyRight { }
  case object Colon extends ReplToken(":", <*--|)
  case object Comma extends ReplToken(",", <*--|) with InsistCozyLeft { }
  case object Dot extends ReplToken(".", <*>) with InsistCozy { }
  case object Eof extends ReplToken("EOF")
  case object ErrorToken extends ReplToken("<internal error>")
  case object False extends ReplToken("false")
  case object Import extends ReplToken("import")
  case object LBrace extends ReplToken("{") with InsistSpaced { }
  case object LBracket extends ReplToken("[") with Brackets { }
  case object LParen extends ReplToken("(", |--*>)
  case object Newline extends ReplToken("\n", <*>) with InsistCozy { override def rawString = "<NL>\n" }
  case object Newlines extends ReplToken("\n\n", <*>) with InsistCozy { override def rawString = "<NLS>\n\n" }
  case object Null extends ReplToken("null")
  case object RBrace extends ReplToken("}", |--*>) with InsistSpaced { }
  case object RBracket extends ReplToken("]") with Brackets { }
  case object RParen extends ReplToken(")", <*--|)
  case object Semi extends ReplToken(";", <*--|)
  case object Subtype extends ReplToken("<:") with InsistSpaced { }
  case object Supertype extends ReplToken(">:") with InsistSpaced { }
  case object True extends ReplToken("true")
  case object ViewBound extends ReplToken("<%") with InsistSpaced { }

  class Tokenizer(in: UnitScanner) {
    private def translate(tokenCode: Int): ReplToken = tokenCode match {
      case IDENTIFIER | BACKQUOTED_IDENT => Id("" + in.name)
      case CHARLIT | INTLIT | LONGLIT    => Lit(in.intVal)
      case DOUBLELIT | FLOATLIT          => Lit(in.floatVal)
      case STRINGLIT                     => Lit(escape(in.strVal))
      case SYMBOLLIT                     => Lit(scala.Symbol(in.strVal))
      case _ =>
        symbolTokenMap.getOrElse(
          tokenCode,
          token2name get tokenCode match {
            case Some(name) => Token(tokenCode)("" + name)
            case _          => Token(tokenCode)("<unknown: " + tokenCode + ">")
          }
        )
    }
    def tokenIterator: Iterator[ReplToken] = (
      Iterator continually {
        try translate(in.token)
        finally in.nextToken()
      } takeWhile (_ ne Eof)
    )
  }

  def prettyPrintRaw(tokens: TraversableOnce[ReplToken]) {
    withRawTokens(prettyPrint(tokens))
  }

  def prettyPrint(tokens: TraversableOnce[ReplToken]) {
    new TokenPrinter prettyPrint tokens
  }

  private class TokenPrinter {
    type TokenTriple = (ReplToken, ReplToken, ReplToken)
    val writer = new Indenter
    var prev: List[ReplToken] = Nil

    def isIdentPart(t: ReplToken) = t match {
      case Dot | Id(_)  => true
      case _            => false
    }
    def prevNonIdent = prev dropWhile isIdentPart match {
      case Nil    => ErrorToken
      case t :: _ => t
    }
    def inImport = prevNonIdent == Import

    def printToken(left: ReplToken, token: ReplToken) = token match {
      case LBrace =>
        writer openIndent (
          if (writer.atStartOfLine) token
          else " " + token
        )
      case RBrace             =>
        writer.closeIndent(token)
      case tok @ (Newline | Newlines) =>
        writer.nextIndent(tok)
      case _                  =>
        writer print (
          if (writer.atStartOfLine) token
          else if (left <--?--> token) token
          else " " + token
        )
    }

    def prettyPrint(tokens: TraversableOnce[ReplToken]) {
      val it = Iterator(Newline) ++ tokens.toIterator ++ Iterator(Newline) sliding 3 map { x =>
        (x: @unchecked) match {
          case List(x1, x2, x3) => ((x1, x2, x3))
        }
      }
      prettyPrint(it)
    }
    def prettyPrint(it: Iterator[TokenTriple]) {
      while (it.hasNext) it.next match {
        // special casing to avoid newline on empty blocks
        case (left, LBrace, RBrace)        =>
          it.next
          writer print " { }"
        // special casing to avoid newlines on import x.{ y, z, q }
        case (left, LBrace, _) if inImport =>
          writer print LBrace
          def loop() {
            if (it.hasNext) {
              val (_, tok, _) = it.next
              if (tok != Comma) {
                writer print " "
              }
              writer print tok
              if (tok != RBrace)
                loop()
            }
          }
          loop()
        case (left, token, right) =>
          printToken(left, token)

          if (it.hasNext) prev ::= token
          else printToken(token, right)
      }
    }
  }
}
