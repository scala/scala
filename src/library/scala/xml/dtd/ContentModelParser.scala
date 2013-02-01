/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.xml
package dtd

/** Parser for regexps (content models in DTD element declarations) */

object ContentModelParser extends Scanner { // a bit too permissive concerning #PCDATA
  import ContentModel._

  /** parses the argument to a regexp */
  def parse(s: String): ContentModel = { initScanner(s); contentspec }

  def accept(tok: Int) = {
    if (token != tok) {
      if ((tok == STAR) && (token == END))                  // common mistake
        scala.sys.error("in DTDs, \n"+
              "mixed content models must be like (#PCDATA|Name|Name|...)*");
      else
        scala.sys.error("expected "+token2string(tok)+
              ", got unexpected token:"+token2string(token));
    }
    nextToken
  }

  // s [ '+' | '*' | '?' ]
  def maybeSuffix(s: RegExp) = token match {
    case STAR => nextToken; Star(s)
    case PLUS => nextToken; Sequ(s, Star(s))
    case OPT  => nextToken; Alt(Eps, s)
    case _    => s
  }

  // contentspec ::= EMPTY | ANY | (#PCDATA) | "(#PCDATA|"regexp)

  def contentspec: ContentModel = token match {

    case NAME => value match {
      case "ANY"   => ANY
      case "EMPTY" => EMPTY
      case _       => scala.sys.error("expected ANY, EMPTY or '(' instead of " + value );
    }
    case LPAREN =>

      nextToken;
      sOpt;
      if (token != TOKEN_PCDATA)
        ELEMENTS(regexp);
      else {
        nextToken;
        token match {
        case RPAREN =>
          PCDATA
        case CHOICE =>
          val res = MIXED(choiceRest(Eps));
          sOpt;
          accept( RPAREN );
          accept( STAR );
          res
        case _ =>
          scala.sys.error("unexpected token:" + token2string(token) );
        }
      }

    case _ =>
      scala.sys.error("unexpected token:" + token2string(token) );
    }
  //                                  sopt ::= S?
  def sOpt() = if( token == S ) nextToken;

  //                      (' S? mixed ::= '#PCDATA' S? ')'
  //                                    | '#PCDATA' (S? '|' S? atom)* S? ')*'

  //       '(' S? regexp ::= cp S? [seqRest|choiceRest] ')' [ '+' | '*' | '?' ]
  def regexp: RegExp = {
    val p = particle;
    sOpt;
    maybeSuffix(token match {
      case RPAREN  => nextToken; p
      case CHOICE  => val q = choiceRest( p );accept( RPAREN ); q
      case COMMA   => val q = seqRest( p );   accept( RPAREN ); q
    })
  }

  //                                             seqRest ::= (',' S? cp S?)+
  def seqRest(p: RegExp) = {
    var k = List(p);
    while( token == COMMA ) {
      nextToken;
      sOpt;
      k = particle::k;
      sOpt;
    }
    Sequ( k.reverse:_* )
  }

  //                                          choiceRest ::= ('|' S? cp S?)+
  def choiceRest( p:RegExp ) = {
    var k = List( p );
    while( token == CHOICE ) {
      nextToken;
      sOpt;
      k = particle::k;
      sOpt;
    }
    Alt( k.reverse:_* )
  }

  //                                  particle ::=  '(' S? regexp
  //                                             |  name [ '+' | '*' | '?' ]
  def particle = token match {
    case LPAREN => nextToken; sOpt; regexp;
    case NAME   => val a = Letter(ElemName(value)); nextToken; maybeSuffix(a)
    case _      => scala.sys.error("expected '(' or Name, got:"+token2string(token));
  }

  //                                     atom ::= name
  def atom = token match {
    case NAME   => val a = Letter(ElemName(value)); nextToken; a
    case _      => scala.sys.error("expected Name, got:"+token2string(token));
  }
}
