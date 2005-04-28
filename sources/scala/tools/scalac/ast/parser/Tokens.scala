/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

import scalac.util.Name;

package scala.tools.scalac.ast.parser {

object Tokens {

  /** The token names */
  private var tokenName = new Array[Name](128);
  private var numToken = 0;

  /** Keyword array; maps from name indices to tokens */
  private var key: Array[byte] = _;
  private var maxKey = 0;

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
  val IMPLICIT = 45;

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
  val VIEWBOUND = 73;

  /** parenthesis */
  val LPAREN = 90;
  val RPAREN = 91;
  val LBRACKET = 92;
  val RBRACKET = 93;
  val LBRACE = 94;
  val RBRACE = 95;

  /** XML mode */
  val XMLSTART = 96;

  /** Returns true if argument corresponds to a keyword. */
  def isKeyword(str: String) = Name.fromString(str).index <= maxKey;

  /** Returns the token corresponding to given name. */
  def name2token(name: Name): int =
    if (name.index <= maxKey) key(name.index) else IDENTIFIER;

  /** Returns the string representation of given token. */
  def token2string(token: int): String = token match {
    case IDENTIFIER =>
      "identifier"/* + \""+name+"\""*/
    case CHARLIT =>
      "character literal"
    case INTLIT =>
      "integer literal"
    case LONGLIT =>
      "long literal"
    case FLOATLIT =>
      "float literal"
    case DOUBLELIT =>
      "double literal"
    case STRINGLIT =>
      "string literal"
    case SYMBOLLIT =>
      "symbol literal"
    case LPAREN =>
      "'('"
    case RPAREN =>
      "')'"
    case LBRACE =>
      "'{'"
    case RBRACE =>
      "'}'"
    case LBRACKET =>
      "'['"
    case RBRACKET =>
      "']'"
    case EOF =>
      "eof"
    case ERROR =>
      "something"
    case SEMI =>
      "';'"
    case COMMA =>
      "','"
    case CASECLASS =>
      "case class"
    case CASEOBJECT =>
      "case object"
    case XMLSTART =>
      "$XML<"
    case _ =>
      try {
        "'" + tokenName(token).toString() + "'"
      } catch {
	case _: ArrayIndexOutOfBoundsException =>
          "'<" + token + ">'"
	case _: NullPointerException =>
          "'<(" + token + ")>'"
      }
  }

  // Enter keywords
  enterKeyword("abstract", ABSTRACT);
  enterKeyword("case", CASE);
  enterKeyword("class", CLASS);
  enterKeyword("catch", CATCH);
  enterKeyword("def", DEF);
  enterKeyword("do", DO);
  enterKeyword("else", ELSE);
  enterKeyword("extends", EXTENDS);
  enterKeyword("false", FALSE);
  enterKeyword("final", FINAL);
  enterKeyword("finally", FINALLY);
  enterKeyword("for", FOR);
  enterKeyword("if", IF);
  enterKeyword("implicit", IMPLICIT);
  enterKeyword("import", IMPORT);
  enterKeyword("new", NEW);
  enterKeyword("null", NULL);
  enterKeyword("object", OBJECT);
  enterKeyword("override", OVERRIDE);
  enterKeyword("package", PACKAGE);
  enterKeyword("private", PRIVATE);
  enterKeyword("protected", PROTECTED);
  enterKeyword("return", RETURN);
  enterKeyword("sealed", SEALED);
  enterKeyword("super", SUPER);
  enterKeyword("this", THIS);
  enterKeyword("throw", THROW);
  enterKeyword("trait", TRAIT);
  enterKeyword("true", TRUE);
  enterKeyword("try", TRY);
  enterKeyword("type", TYPE);
  enterKeyword("val", VAL);
  enterKeyword("var", VAR);
  enterKeyword("with", WITH);
  enterKeyword("while", WHILE);
  enterKeyword("yield", YIELD);
  enterKeyword(".", DOT);
  enterKeyword("_", USCORE);
  enterKeyword(":", COLON);
  enterKeyword("=", EQUALS);
  enterKeyword("=>", ARROW);
  enterKeyword("<-", LARROW);
  enterKeyword("<:", SUBTYPE);
  enterKeyword(">:", SUPERTYPE);
  enterKeyword("<%", VIEWBOUND);
  enterKeyword("#", HASH);
  enterKeyword("@", AT);

  // Build keyword array
  key = new Array[byte](maxKey+1);
  for (val i <- Iterator.range(0, maxKey))
    key(i) = IDENTIFIER;
  for (val j <- Iterator.range(0, numToken))
    if (tokenName(j) != null) key(tokenName(j).index) = j.asInstanceOf[byte];

  private def enterKeyword(s: String, tokenId: int) = {
    while (tokenId > tokenName.length) {
      val newTokName = new Array[Name](tokenName.length * 2);
      System.arraycopy(tokenName, 0, newTokName, 0, newTokName.length);
      tokenName = newTokName;
    }
    val n = Name.fromString(s);
    tokenName(tokenId) = n;
    if (n.index > maxKey) maxKey = n.index;
    if (tokenId >= numToken) numToken = tokenId + 1;
  }

}

}
