/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package ast.parser

/** Common code between Scala's Tokens and JavaTokens. */
abstract class CommonTokens {

  def isIdentifier(code: Int): Boolean
  def isLiteral(code: Int): Boolean

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

  /** keywords */
  final val NEW = 20
  final val THIS = 21
  final val SUPER = 23

  final val NULL = 24
  final val TRUE = 25
  final val FALSE = 26

  // J: INSTANCEOF = 27
  // J: CONST = 28

  /** modifiers */
  // S: IMPLICIT = 40
  // S: OVERRIDE = 41
  // J: PUBLIC = 42
  final val PROTECTED = 43
  final val PRIVATE = 44
  final val SEALED = 45     // J: contextual keyword
  final val ABSTRACT = 46
  // J: DEFAULT = 47
  // J: STATIC = 48
  final val FINAL = 49
  // J: TRANSIENT = 50
  // J: VOLATILE = 51
  // J: SYNCHRONIZED = 52
  // J: NATIVE = 53
  // J: STRICTFP = 54
  // S: LAZY = 55
  // J: THROWS = 56
  // S: MACRO = 57

  /** templates */
  final val PACKAGE = 60
  final val IMPORT = 61
  final val CLASS = 62
  // S: CASECLASS = 63
  // S: OBJECT = 64
  // S: CASEOBJECT = 65
  // S: TRAIT, J: INTERFACE = 66
  // J: ENUM = 67
  final val EXTENDS = 68
  // S: WITH, J: IMPLEMENTS = 69
  // S: TYPE = 70
  // S: FORSOME = 71
  // S: DEF = 72
  // S: VAL = 73
  // S: VAR = 74

  /** control structures */
  final val IF = 80
  // S: THEN = 81
  final val ELSE = 82
  final val WHILE = 83
  final val DO = 84
  final val FOR = 85
  // S: YIELD = 86
  // J: BREAK = 87
  // J: CONTINUE = 88
  // J: GOTO = 89
  final val THROW = 90
  final val TRY = 91
  final val CATCH = 92
  final val FINALLY = 93
  // J: SWITCH = 94
  // S: MATCH = 95
  final val CASE = 96
  final val RETURN = 97
  // J: ASSERT = 98

  /** parenthesis */
  final val LPAREN = 100
  final val RPAREN = 101
  final val LBRACKET = 102
  final val RBRACKET = 103
  final val LBRACE = 104
  final val RBRACE = 105

  /** special symbols */
  final val COMMA = 120
  final val SEMI = 121
  final val DOT = 122
  final val COLON = 123
  final val EQUALS = 124
  final val AT = 125
  // S: <special symbols> = 130 - 139
  // J: <special symbols> = 140 - 179
  // J: <primitive types> = 180 - 189
}
