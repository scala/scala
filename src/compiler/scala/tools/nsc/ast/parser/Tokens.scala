/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc
package ast.parser

object Tokens {

  /** special tokens */
  final val EMPTY = -3
  final val UNDEF = -2
  final val ERROR = -1
  final val EOF = 0

  /** literals */
  final val CHARLIT = 1
  final val INTLIT = 2
  final val LONGLIT = 3
  final val FLOATLIT = 4
  final val DOUBLELIT = 5
  final val STRINGLIT = 6
  final val SYMBOLLIT = 7
  def isLiteral(code : Int) =
    code >= CHARLIT && code <= SYMBOLLIT

  /** identifiers */
  final val IDENTIFIER = 10
  final val BACKQUOTED_IDENT = 11
  def isIdentifier(code : Int) =
    code >= IDENTIFIER && code <= BACKQUOTED_IDENT

  def canBeginExpression(code : Int) = code match {
  case IDENTIFIER|BACKQUOTED_IDENT|USCORE => true
  case LBRACE|LPAREN|LBRACKET|COMMENT|STRINGLIT => true
  case IF|DO|WHILE|FOR|NEW|TRY|THROW => true
  case NULL|THIS|TRUE|FALSE => true
  case code if isLiteral(code) => true
  case _ => false
  }


  /** keywords */
  final val IF = 20
  final val FOR = 21
  final val ELSE = 22
  final val THIS = 23
  final val NULL = 24
  final val NEW = 25
  final val WITH = 26
  final val SUPER = 27
  final val CASE = 28
  final val CASECLASS = 29
  final val CASEOBJECT = 30
  final val VAL = 31
  final val ABSTRACT = 32
  final val FINAL = 33
  final val PRIVATE = 34
  final val PROTECTED = 35
  final val OVERRIDE = 36
  final val IMPLICIT = 37
  final val VAR = 38
  final val DEF = 39
  final val TYPE = 40
  final val EXTENDS = 41
  final val TRUE = 42
  final val FALSE = 43
  final val OBJECT = 44
  final val CLASS = 45

  final val IMPORT = 46
  final val PACKAGE = 47
  final val YIELD = 48
  final val DO = 49
  final val TRAIT = 50
  final val SEALED = 51
  final val THROW = 52
  final val TRY = 53
  final val CATCH = 54
  final val FINALLY = 55
  final val WHILE = 56
  final val RETURN = 57
  final val MATCH = 58
  final val FORSOME = 59
  final val REQUIRES = 60
  final val LAZY = 61

  def isKeyword(code : Int) =
    code >= IF && code <= LAZY

  def isDefinition(code : Int) = code match {
  case CLASS|TRAIT|OBJECT => true
  case CASECLASS|CASEOBJECT => true
  case DEF|VAL|VAR => true
  case TYPE => true
  case _ => false
  }


  /** special symbols */
  final val COMMA = 70
  final val SEMI = 71
  final val DOT = 72
  final val USCORE = 73
  final val COLON = 74
  final val EQUALS = 75
  final val LARROW = 76
  final val ARROW = 77
  final val NEWLINE = 78
  final val NEWLINES = 79
  final val SUBTYPE = 80
  final val SUPERTYPE = 81
  final val HASH = 82
  final val AT = 83
  final val VIEWBOUND = 84

  def isSymbol(code : Int) =
    code >= COMMA && code <= VIEWBOUND

  /** parenthesis */
  final val LPAREN = 90
  final val RPAREN = 91
  final val LBRACKET = 92
  final val RBRACKET = 93
  final val LBRACE = 94
  final val RBRACE = 95

  def isBrace(code : Int) =
    code >= LPAREN && code <= RBRACE
  def isOpenBrace(code : Int) = isBrace(code) && (code % 2 == 0)
  def isCloseBrace(code : Int) = isBrace(code) && (code % 2 == 1)

  /** XML mode */
  final val XMLSTART = 96

  /** for IDE only */
  final val COMMENT = 97

  final val WHITESPACE = 105
  final val IGNORE = 106
  final val ESCAPE = 109

  def isSpace(at : Char) = at match {
  case ' ' | '\t' => true
  case _ => false
  }
  import scala.tools.nsc.util.SourceFile._

  def isNewLine(at : Char) = at match {
  case CR | LF | FF => true
  case _ => false
  }
}
