/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.ast.parser;

object Tokens {

  /** special tokens */
  val EMPTY = -3;
  val UNDEF = -2;
  val ERROR = -1;
  val EOF = 0;

  /** literals */
  val CHARLIT = 1;
  val INTLIT = 2;
  val LONGLIT = 3;
  val FLOATLIT = 4;
  val DOUBLELIT = 5;
  val STRINGLIT = 6;
  val SYMBOLLIT = 7;

  /** identifier */
  val IDENTIFIER = 10;

  /** keywords */
  val IF = 20;
  val FOR = 21;
  val ELSE = 22;
  val THIS = 23;
  val NULL = 24;
  val NEW = 25;
  val WITH = 26;
  val SUPER = 27;
  val CASE = 28;
  val CASECLASS = 29;
  val CASEOBJECT = 30;
  val VAL = 31;
  val ABSTRACT = 32;
  val FINAL = 33;
  val PRIVATE = 34;
  val PROTECTED = 35;
  val OVERRIDE = 36;
  val VAR = 37;
  val DEF = 38;
  val TYPE = 39;
  val EXTENDS = 40;
  val TRUE = 41;
  val FALSE = 42;
  val OBJECT = 43;
  val CLASS = 44;

  val IMPORT = 46;
  val PACKAGE = 47;
  val YIELD = 48;
  val DO = 49;
  val TRAIT = 50;
  val SEALED = 51;
  val THROW = 52;
  val TRY = 53;
  val CATCH = 54;
  val FINALLY = 55;
  val WHILE = 56;
  val RETURN = 57;

  /** special symbols */
  val COMMA = 61;
  val SEMI = 62;
  val DOT = 63;
  val USCORE = 64;
  val COLON = 65;
  val EQUALS = 66;
  val LARROW = 67;
  val ARROW = 68;
  val SUBTYPE = 69;
  val SUPERTYPE = 70;
  val HASH = 71;
  val AT = 72;

  /** parenthesis */
  val LPAREN = 90;
  val RPAREN = 91;
  val LBRACKET = 92;
  val RBRACKET = 93;
  val LBRACE = 94;
  val RBRACE = 95;
}
