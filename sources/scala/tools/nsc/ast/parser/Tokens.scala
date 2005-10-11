/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.ast.parser;

object Tokens {

  /** special tokens */
  final val EMPTY = -3;
  final val UNDEF = -2;
  final val ERROR = -1;
  final val EOF = 0;

  /** literals */
  final val CHARLIT = 1;
  final val INTLIT = 2;
  final val LONGLIT = 3;
  final val FLOATLIT = 4;
  final val DOUBLELIT = 5;
  final val STRINGLIT = 6;
  final val SYMBOLLIT = 7;

  /** identifier */
  final val IDENTIFIER = 10;

  /** keywords */
  final val IF = 20;
  final val FOR = 21;
  final val ELSE = 22;
  final val THIS = 23;
  final val NULL = 24;
  final val NEW = 25;
  final val WITH = 26;
  final val SUPER = 27;
  final val CASE = 28;
  final val CASECLASS = 29;
  final val CASEOBJECT = 30;
  final val VAL = 31;
  final val ABSTRACT = 32;
  final val FINAL = 33;
  final val PRIVATE = 34;
  final val PROTECTED = 35;
  final val OVERRIDE = 36;
  final val IMPLICIT = 37;
  final val VAR = 38;
  final val DEF = 39;
  final val TYPE = 40;
  final val EXTENDS = 41;
  final val TRUE = 42;
  final val FALSE = 43;
  final val OBJECT = 44;
  final val CLASS = 45;

  final val IMPORT = 46;
  final val PACKAGE = 47;
  final val YIELD = 48;
  final val DO = 49;
  final val TRAIT = 50;
  final val SEALED = 51;
  final val THROW = 52;
  final val TRY = 53;
  final val CATCH = 54;
  final val FINALLY = 55;
  final val WHILE = 56;
  final val RETURN = 57;
  final val MATCH = 58;
  final val REQUIRES = 59;

  /** special symbols */
  final val COMMA = 61;
  final val SEMI = 62;
  final val DOT = 63;
  final val USCORE = 64;
  final val COLON = 65;
  final val EQUALS = 66;
  final val LARROW = 67;
  final val ARROW = 68;
  final val SUBTYPE = 70;
  final val SUPERTYPE = 71;
  final val HASH = 72;
  final val AT = 73;
  final val VIEWBOUND = 74; //todo: elim?

  /** parenthesis */
  final val LPAREN = 90;
  final val RPAREN = 91;
  final val LBRACKET = 92;
  final val RBRACKET = 93;
  final val LBRACE = 94;
  final val RBRACE = 95;
}
