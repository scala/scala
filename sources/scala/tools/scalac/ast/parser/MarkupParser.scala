/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002-2004, LAMP/EPFL         **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

import scalac.ast._;
import scalac.atree.AConstant;
import scalac._;
import scalac.symtab.Modifiers;
import scala.tools.util.Position;
import java.lang.{Integer, Long, Float, Double};
import scala.Iterator;
import scala.tools.scalac.util.NewArray;
import scala.collection.immutable.ListMap ;
import scala.collection.mutable.Buffer;
import scala.xml.{Text,TextBuffer};
import scalac.util.Name;

package scala.tools.scalac.ast.parser {

class MarkupParser( unit:Unit, s:Scanner, p:Parser, preserveWS:boolean ) {

  import Tokens.{EMPTY, LBRACE, RBRACE} ;
  import scala.tools.scalac.ast.{TreeList => myTreeList}

  /** the XML tree factory */
  val mk = new SymbolicXMLBuilder( unit.global.make, unit.global.treeGen, p, preserveWS );

  /** the XML tree builder */
  val gen = unit.global.treeGen ;

  var mode:boolean = false;
  final val PATTERN = true;
  final val EXPR    = false;

  /** xLiteral = xExpr { xExpr }
   * @return Scala representation of this xml literal
   * precondition: s.xStartsXML == true
  */
  def xLiteral:Tree = {
    mode = EXPR;
    val pos = s.pos;
    var tree = xExpr; s.token = EMPTY; s.nextToken();
    if( s.xStartsXML )  {
      val ts = new myTreeList(); ts.append( tree );
      while( s.xStartsXML ) { ts.append( xExpr ); s.nextToken(); }
      tree = mk.makeXMLseq( pos, ts.toArray() );
    }
    tree
  }

  /** parse attribute and add it to listmap
   *  [41] Attributes    ::= { S Name Eq AttValue }
   *       AttValue     ::= `'` { _  } `'`
   *                      | `"` { _ } `"`
   *                      | `{` scalablock `}`
  */
  def xAttributes = {
    var aMap = new ListMap[String, Tree];
    while( xml.Parsing.isNameStart( s.ch )) {
      val key = s.xName.toString();
      s.xEQ;
      val delim = s.ch;
      val value:Tree = s.ch match {
        case '"' | '\'' =>
          val pos = s.pos;
          s.xNext;
          val tmp = s.xAttributeValue( delim );
          s.xNext;
          gen.mkStringLit( pos, tmp )
        case '{' =>
          s.xNext;
          xScalaExpr;
        case _ =>
	  s.xSyntaxError( "' or \" delimited attribute value or '{' scala-expr '}' expected" );
          gen.mkStringLit( s.pos, "<syntax-error>" )
      };
      // well-formedness constraint: unique attribute names
      if( aMap.contains( key ))
        s.xSyntaxError( "attribute "+key+" may only be defined once" );
      aMap = aMap.update( key, value );
      if(( s.ch != '/' )&&( s.ch != '>' ))
        s.xSpace;
    };
   aMap
  }

  /** parse a start or empty tag.
   *  [40] STag         ::= '<' Name { S Attribute } [S]
   *  [44] EmptyElemTag ::= '<' Name { S Attribute } [S]
   */
  def xTag = {
    val elemName = s.xName;
    s.xSpaceOpt;
    val aMap = if(xml.Parsing.isNameStart( s.ch )) {
      xAttributes;
    } else {
      ListMap.Empty[String,Tree];
    }
    Tuple2( elemName, aMap );
  }

  /* [42]  '<' xmlEndTag ::=  '<' '/' Name S? '>'                 */
  def xEndTag( n:Name ) = {
    s.xToken('/');
    val m = s.xName;
    if(n != m) s.xSyntaxError( "expected closing tag of " + n/* +", not "+m*/);
    s.xSpaceOpt;
    s.xToken('>')
  }

  def xScalaExpr:Tree = {
    s.xSync;
    val b = p.expr(true,false);
    if(s.token != RBRACE)
      s.xSyntaxError(" expected end of Scala block");
    return b
  }

  /** '<' xExpr ::= xmlTag1 '>'  { xmlExpr | '{' simpleExpr '}' } ETag
   *               | xmlTag1 '/' '>'
   *  the caller has to resynchronize with s.token = EMPTY; s.nextToken;
   */
  def xExpr:Tree = {
    var pos = s.pos;
    val Tuple2(qname:Name, attrMap:ListMap[String,Tree]) = xTag;
    if(s.ch == '/') { // empty element
      s.xToken('/');
      s.xToken('>');
      mk.makeXML( pos, qname, attrMap, Tree.EMPTY_ARRAY );
    } else { // handle content
      s.xToken('>');
      val ts = new myTreeList();
      var exit = false;
      while( !exit ) {
        if( s.xScalaBlock ) {
          ts.append( xScalaExpr );
        } else {
          pos = s.pos;
          s.ch match {
            case '<' => // another tag
              s.xNext;
              s.ch match {
                case '/' => exit = true;            // end tag
                case '!' =>
                  s.xNext;
                  if( '[' == s.ch )                 // CDATA
                    ts.append( mk.CharData( pos, s.xCharData ));
                  else                              // comment
                    ts.append( mk.Comment( pos, s.xComment ));
                case '?' =>                         // PI
                  s.xNext;
                  ts.append( mk.ProcInstr( pos, s.xProcInstr ));
                case _   => ts.append( xExpr );     // child
              }

            case '{' =>
              if( s.xCheckScalaBlock ) {
                ts.append( xScalaExpr );
              } else {
                val str = new StringBuffer("{");
                str.append( s.xText );
                mk.appendTrimmed( pos, mode, ts, str.toString() )
              }
            // postcond: s.xScalaBlock == false!
            case '&' => // EntityRef or CharRef
              s.xNext;
              s.ch match {
                case '#' => // CharacterRef
                  s.xNext;
                  val theChar = mk.makeText( s.pos, false, s.xCharRef );
                  s.xToken(';');
                  ts.append( theChar );
                case _ => // EntityRef
                  val n = s.xName ;
                  s.xToken(';');
                  ts.append( mk.EntityRef( pos, n ));
              }
            case _ => // text content
              mk.appendTrimmed( pos, mode, ts, s.xText );
            // here s.xScalaBlock might be true

          }
        }
      }
      xEndTag( qname );
      mk.makeXML( pos, qname, attrMap, ts.toArray() );
    }
  }

  /** @see xmlPattern. resynchronizes after succesful parse
  def xLiteralPattern = {
    val t = xPattern; s.nextToken(); t
  }
  */

  /** @see xmlPattern. resynchronizes after succesful parse
   * @return this xml pattern
   * precondition: s.xStartsXML == true
  */
  def xLiteralPattern:Tree = {
    val oldMode = mode;
    mode = PATTERN;
    val pos = s.pos;
    var tree = xPattern; s.token = EMPTY; s.nextToken();
    if( s.xStartsXML )  {
      val ts = new myTreeList(); ts.append( tree );
      while( s.xStartsXML ) { ts.append( xPattern ); s.nextToken(); }
      tree = mk.makeXMLseqPat( pos, ts.toArray() );
    }
    mode = oldMode;
    tree
  }


  /** xScalaPatterns  ::= patterns
   */
  def xScalaPatterns:Array[Tree] = {
    s.xSync;
    val b = p.patterns();
    if( s.token != RBRACE )
      s.xSyntaxError(" expected end of Scala patterns");
    return b
  }


  /** '<' xPattern  ::= Name [S] { xmlPattern | '{' pattern3 '}' } ETag
   *                  | Name [S] '/' '>'
   */
  def xPattern:Tree = {
    //Console.println("xPattern");
    val pos = s.pos;
    val qname = s.xName;
    s.xSpaceOpt;
    if( s.ch == '/' ) { // empty tag
      s.xNext;
      s.xToken('>');
      return mk.makeXMLpat( pos, qname, Tree.EMPTY_ARRAY );
    };

     // else: tag with content
    s.xToken('>');
    val ts = new myTreeList();
    var exit = false;
    while( !exit ) {
      if( s.xScalaBlock ) {
        ts.append( xScalaPatterns );
      } else
        s.ch match {
          case '<' => { // tag
            s.xNext;
            if( s.ch != '/' ) { //child
              ts.append( xPattern );
            } else {
              exit = true
            }
          }
          case '{' => // embedded Scala patterns
            while( s.ch == '{' ) {
              s.nextch();
              ts.append( xScalaPatterns );
            }
            // postcond: s.xScalaBlock = false;
          if( s.xScalaBlock ) throw new ApplicationError(); // assert
          case _ => // text
            mk.appendTrimmed( pos, mode, ts, s.xText );
          // here  s.xScalaBlock might be true;
          //if( s.xScalaBlock ) throw new ApplicationError("after:"+text); // assert
	}
    }
    xEndTag( qname );
    mk.makeXMLpat( pos, qname, ts.toArray() );
  }

} /* class MarkupParser */
}
