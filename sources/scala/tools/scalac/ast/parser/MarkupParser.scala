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
import scala.collection.mutable;
import scala.xml.{Text,TextBuffer};

import scala.xml.parsing.AttribValue ;
package scala.tools.scalac.ast.parser {

class MarkupParser(unit: CompilationUnit, s: Scanner, p: Parser, presWS: boolean ) with scala.xml.parsing.MarkupParser[Tree,Tree] {

  import Tokens.{EMPTY, LBRACE, RBRACE} ;
  import scala.tools.scalac.ast.{TreeList => myTreeList}

  final val preserveWS = presWS;

  /** the XML tree factory */
  val handle: SymbolicXMLBuilder = new SymbolicXMLBuilder( unit.global.make, unit.global.treeGen, p, presWS );

  //override type MarkupType = handle.MarkupType;
  //override type AVType     = handle.AVType;

  /** the XML tree builder */
  val gen = unit.global.treeGen ;

  final val PATTERN = true;
  final val EXPR    = false;

  //val cbuf = new StringBuffer();

  /** append Unicode character to name buffer*/
  //private def putChar(c: char) = cbuf.append( c );

  /** xLiteral = element { element }
   * @return Scala representation of this xml literal
   * precondition: s.xStartsXML == true
  */
  def xLiteral: Tree = {
    init;
    handle.isPattern = false;
    val pos = s.pos;
    var tree = element; xSpaceOpt;
    if( ch=='<' )  {
      val ts = new mutable.ArrayBuffer[Tree](); ts.append( tree );
      while( ch == '<' ) {
        nextch;
        ts.append( element );
        xSpaceOpt;
      }
      tree = handle.makeXMLseq( pos, ts );
    }
    //Console.println("out of xLiteral, parsed:"+tree.toString());
    s.xSync2;
    tree
  }

  /** @see xmlPattern. resynchronizes after succesful parse
   * @return this xml pattern
   * precondition: s.xStartsXML == true
  */
  def xLiteralPattern:Tree = {
    init;
    val oldMode = handle.isPattern;
    handle.isPattern = true;
    val pos = s.pos;
    var tree = xPattern; xSpaceOpt;
    if( ch == '<' )  {
      val ts = new myTreeList(); ts.append( tree );
      while( ch == '<' && lookahead != '-' ) {
        nextch;
        ts.append( xPattern );
        xSpaceOpt;
      }
      tree = handle.makeXMLseqPat( pos, ts.toArray() );
    }
    handle.isPattern = oldMode;
    //Console.println("out of xLiteralPattern, parsed:"+tree.toString());
    s.xSync2;
    tree
  }

  def xEmbeddedExpr:Tree = {
    sync;
    val b = p.expr(true,false);
    if(s.token != RBRACE)
      xSyntaxError(" expected end of Scala block");
    init;
    //Console.println("[out of xScalaExpr s.ch = "+s.ch+" ch="+ch+"]");
    return b
  }

  /** xScalaPatterns  ::= patterns
   */
  def xScalaPatterns:Array[Tree] = {
    sync;
    val b = p.patterns();
    if( s.token != RBRACE )
      xSyntaxError(" expected end of Scala patterns");
    init;
    return b
  }

  //var ch: Char = _;

  /** this method assign the next character to ch and advances in input */
  def nextch: Unit = { s.xNext; ch = s.ch; pos = s.pos; }

  def lookahead = { s.xLookahead }

  def init: Unit = {
    ch = s.ch;
    //Console.println("\ninit! ch = "+ch);
  }

  /** parse attribute and add it to listmap
   *  [41] Attributes    ::= { S Name Eq AttValue }
   *       AttValue     ::= `'` { _  } `'`
   *                      | `"` { _ } `"`
   *                      | `{` scalablock `}`
  def xAttributes = {
    var aMap = new mutable.HashMap[String, AttribValue[Tree]];
    while( xml.Parsing.isNameStart( ch )) {
      val key = xName;
      xEQ;
      val delim = ch;
      val value:Tree = ch match {
        case '"' | '\'' =>
          val pos1 = pos;
          nextch;
          val tmp = xAttributeValue( delim );
          nextch;
          gen.mkStringLit( pos1, tmp )
        case '{' =>
          nextch;
          xEmbeddedExpr;
        case _ =>
	  xSyntaxError( "' or \" delimited attribute value or '{' scala-expr '}' expected" );
          gen.mkStringLit( pos, "<syntax-error>" )
      };
      // well-formedness constraint: unique attribute names
      if( aMap.contains( key ))
        xSyntaxError( "attribute "+key+" may only be defined once" );
      aMap.update( key, AttribValue( value ));
      if(( ch != '/' )&&( ch != '>' ))
        xSpace;
    };
   aMap
  }
  */


  def xSyntaxError(str:String) = {
    s.syntaxError("in XML literal: "+str);
    nextch;
  }

  def sync: Unit = {
    xEmbeddedBlock = false;
    s.xSync;
  }

  /** '<' xPattern  ::= Name [S] { xmlPattern | '{' pattern3 '}' } ETag
   *                  | Name [S] '/' '>'
   */
  def xPattern:Tree = {
    //Console.println("xPattern");
    val pos1 = pos;
    val qname = xName;
    xSpaceOpt;
    if( ch == '/' ) { // empty tag
      nextch;
      xToken('>');
      return handle.makeXMLpat( pos1, qname, new mutable.ArrayBuffer[Tree]() );
    };

     // else: tag with content
    xToken('>');
    var ts = new mutable.ArrayBuffer[Tree];
    var exit = false;
    while( !exit ) {
      val pos2 = pos;
      if( xEmbeddedBlock ) {
        ts ++ xScalaPatterns;
      } else
        ch match {
          case '<' => { // tag
            nextch;
            if( ch != '/' ) { //child
              ts.append( xPattern );
            } else {
              exit = true
            }
          }
          case '{' => // embedded Scala patterns
            while( ch == '{' ) {
              s.nextch();
              ts ++ xScalaPatterns;
            }
            // postcond: xEmbeddedBlock = false;
          if( xEmbeddedBlock ) throw new ApplicationError(); // assert
          case _ => // teMaxt
            appendText( pos2, ts, xText );
          // here  xEmbeddedBlock might be true;
          //if( xEmbeddedBlock ) throw new ApplicationError("after:"+text); // assert
	}
    }
    xEndTag( qname );
    handle.makeXMLpat( pos1, qname, ts );
  }


  override def appendText(pos: int, ts:mutable.Buffer[Tree], txt:String):Unit = {
    if( !preserveWS )
      for( val t <- TextBuffer.fromString( txt ).toText ) {
        ts.append( handle.text( pos, t.text ) );
      }
    else
      ts.append( handle.text( pos, txt ));
  }
} /* class MarkupParser */
}
