/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package ast.parser

/** Common code between JavaTokens and Tokens.  Not as much (and not as concrete)
 *  as one might like because JavaTokens for no clear reason chose new numbers for
 *  identical token sets.
 */
abstract class Tokens {
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

  def LPAREN: Int
  def RBRACE: Int

  def isIdentifier(code: Int): Boolean
  def isLiteral(code: Int): Boolean
}

object Tokens extends Tokens {
  final val STRINGPART = 7  // a part of an interpolated string
  final val SYMBOLLIT = 8
  final val INTERPOLATIONID = 9 // the lead identifier of an interpolated string

  def isLiteral(code: Int) =
    code >= CHARLIT && code <= INTERPOLATIONID

  /** identifiers */
  final val IDENTIFIER = 10
  final val BACKQUOTED_IDENT = 11
  def isIdentifier(code: Int) = code >= IDENTIFIER && code <= BACKQUOTED_IDENT // used by ide

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
  final val LAZY = 61
  final val MACRO = 62 // not yet used in 2.10
  final val THEN = 63  // not yet used in 2.10

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

  /** parenthesis */
  final val LPAREN = 90
  final val RPAREN = 91
  final val LBRACKET = 92
  final val RBRACKET = 93
  final val LBRACE = 94
  final val RBRACE = 95

  /** XML mode */
  final val XMLSTART = 96

  /** for IDE only */
  final val COMMENT = 97

  final val WHITESPACE = 105
  final val IGNORE = 106
  final val ESCAPE = 109
  
  final val LBRACE_DOT      = 130
  final val LBRACE_DOT3     = 131
  final val LBRACE_QMARK    = 132
  final val LBRACE_EMARK    = 133
  final val LBRACE_ASTERISK = 134
  final val LBRACE_CARET    = 135

  final val RBRACE_DOT      = 141
  final val RBRACE_DOT3     = 142
  final val RBRACE_QMARK    = 143
  final val RBRACE_EMARK    = 144
  final val RBRACE_ASTERISK = 145
  final val RBRACE_CARET    = 146

  final val DOT2            = 150
  final val DOT3            = 151
  
  final val QMARK           = 152
  final val QMARK2          = 153
  
  final val MINUS_SEMI      = 154
  final val BAR_SEMI        = 155
  final val BAR2_SEMI       = 156
  final val BAR_SEMI_BAR    = 157
  final val PERCENT_SEMI    = 158
  
  final val LPAREN_PLUS_RPAREN       = 160
  final val LPAREN_MINUS_RPAREN      = 161
  final val LPAREN_PLUS_MINUS_RPAREN = 162
  final val LPAREN_SEMI_RPAREN       = 163

  final val bracePairs = Map (
    LBRACE_DOT      -> RBRACE_DOT     ,
    LBRACE_DOT3     -> RBRACE_DOT3    ,  
    LBRACE_QMARK    -> RBRACE_QMARK   ,  
    LBRACE_EMARK    -> RBRACE_EMARK   ,  
    LBRACE_ASTERISK -> RBRACE_ASTERISK,  
    LBRACE_CARET    -> RBRACE_CARET   ) 

}
