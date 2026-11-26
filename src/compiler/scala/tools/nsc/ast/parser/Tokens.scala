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

object Tokens extends CommonTokens {
  final val STRINGPART = 7 // a part of an interpolated string
  final val SYMBOLLIT = 8
  final val INTERPOLATIONID = 9 // the lead identifier of an interpolated string

  def isLiteral(code: Int) = code >= CHARLIT && code <= INTERPOLATIONID

  /** identifiers */
  final val IDENTIFIER = 10
  final val BACKQUOTED_IDENT = 11
  def isIdentifier(code: Int) = code == IDENTIFIER || code == BACKQUOTED_IDENT // used by ide

  /** modifiers */
  final val IMPLICIT = 40
  final val OVERRIDE = 41
  final val SEALED = 45
  final val LAZY = 55
  final val MACRO = 57

  /** templates */
  final val CASECLASS = 63
  final val OBJECT = 64
  final val CASEOBJECT = 65
  final val TRAIT = 66
  final val WITH = 69
  final val TYPE = 70
  final val FORSOME = 71
  final val DEF = 72
  final val VAL = 73
  final val VAR = 74

  /** control structures */
  final val THEN = 81
  final val YIELD = 86
  final val MATCH = 95

  /** special symbols */
  final val HASH = 130
  final val USCORE = 131
  final val ARROW = 132
  final val LARROW = 133
  final val SUBTYPE = 134
  final val SUPERTYPE = 135
  final val VIEWBOUND = 136
  final val NEWLINE = 137
  final val NEWLINES = 138
  final val XMLSTART = 139

  /** for IDE only */
  final val COMMENT = 200
  final val WHITESPACE = 201
  final val IGNORE = 202
  final val ESCAPE = 203
}
