/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package javac

object JavaTokens extends ast.parser.Tokens {

  def isLiteral(code : Int) =
    code >= CHARLIT && code <= STRINGLIT

  /** identifiers */
  final val IDENTIFIER = 10
  def isIdentifier(code : Int) =
    code == IDENTIFIER

  /** keywords */
  final val ABSTRACT = 20
  final val ASSERT = 21
  final val BOOLEAN = 22
  final val BREAK = 23
  final val BYTE = 24
  final val CASE = 25
  final val CATCH = 26
  final val CHAR = 27
  final val CLASS = 28
  final val CONST = 29
  final val CONTINUE = 30
  final val DEFAULT = 31
  final val DO = 32
  final val DOUBLE = 33
  final val ELSE = 34
  final val ENUM = 35
  final val EXTENDS = 36
  final val FINAL = 37
  final val FINALLY = 38
  final val FLOAT = 39
  final val FOR = 40
  final val IF = 41
  final val GOTO = 42
  final val IMPLEMENTS = 43
  final val IMPORT = 44
  final val INSTANCEOF = 45
  final val INT = 46
  final val INTERFACE = 47
  final val LONG = 48
  final val NATIVE = 49
  final val NEW = 50
  final val PACKAGE = 51
  final val PRIVATE = 52
  final val PROTECTED = 53
  final val PUBLIC = 54
  final val RETURN = 55
  final val SHORT = 56
  final val STATIC = 57
  final val STRICTFP = 58
  final val SUPER = 59
  final val SWITCH = 60
  final val SYNCHRONIZED = 61
  final val THIS = 62
  final val THROW = 63
  final val THROWS = 64
  final val TRANSIENT = 65
  final val TRY = 66
  final val VOID = 67
  final val VOLATILE = 68
  final val WHILE = 69

  def isKeyword(code : Int) =
    code >= ABSTRACT && code <= WHILE

  /** special symbols */
  final val COMMA = 70
  final val SEMI = 71
  final val DOT = 72
  final val AT = 73
  final val COLON = 74
  final val ASSIGN = 75
  final val EQEQ = 76
  final val BANGEQ = 77
  final val LT = 78
  final val GT = 79
  final val LTEQ = 80
  final val GTEQ = 81
  final val BANG = 82
  final val QMARK = 83
  final val AMP = 84
  final val BAR = 85
  final val PLUS = 86
  final val MINUS = 87
  final val ASTERISK = 88
  final val SLASH = 89
  final val PERCENT = 90
  final val HAT = 91
  final val LTLT = 92
  final val GTGT = 93
  final val GTGTGT = 94
  final val AMPAMP = 95
  final val BARBAR = 96
  final val PLUSPLUS = 97
  final val MINUSMINUS = 98
  final val TILDE = 99
  final val DOTDOTDOT = 100
  final val AMPEQ = 104
  final val BAREQ = 105
  final val PLUSEQ = 106
  final val MINUSEQ = 107
  final val ASTERISKEQ = 1010
  final val SLASHEQ = 109
  final val PERCENTEQ = 110
  final val HATEQ = 111
  final val LTLTEQ = 112
  final val GTGTEQ = 113
  final val GTGTGTEQ = 114

  def isSymbol(code : Int) =
    code >= COMMA && code <= GTGTGTEQ

  /** parenthesis */
  final val LPAREN = 115
  final val RPAREN = 116
  final val LBRACKET = 117
  final val RBRACKET = 118
  final val LBRACE = 119
  final val RBRACE = 120
}
