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

package scala.tools.scalac.ast.parser {

class MarkupParser(unit: CompilationUnit, s: Scanner, p: Parser, trimWS: boolean ) {

  import Tokens.{EMPTY, LBRACE, RBRACE} ;
  import scala.tools.scalac.ast.{TreeList => myTreeList}

  /** the XML tree factory */
  val mk = new SymbolicXMLBuilder( unit.global.make, unit.global.treeGen, p, trimWS );

  /** the XML tree builder */
  val gen = unit.global.treeGen ;

  var mode:boolean = false;
  final val PATTERN = true;
  final val EXPR    = false;

  val cbuf = new StringBuffer();

  /** append Unicode character to name buffer*/
  private def putChar(c: char) = cbuf.append( c );

  /** xLiteral = xExpr { xExpr }
   * @return Scala representation of this xml literal
   * precondition: s.xStartsXML == true
  */
  def xLiteral: Tree = {
    init;
    mode = EXPR;
    val pos = s.pos;
    var tree = xExpr; xSpaceOpt;
    if( ch=='<' )  {
      val ts = new myTreeList(); ts.append( tree );
      while( ch == '<' ) {
        nextch;
        ts.append( xExpr );
        xSpaceOpt;
      }
      tree = mk.makeXMLseq( pos, ts.toArray() );
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
    val oldMode = mode;
    mode = PATTERN;
    val pos = s.pos;
    var tree = xPattern; xSpaceOpt;
    if( ch == '<' )  {
      val ts = new myTreeList(); ts.append( tree );
      while( ch == '<' && lookahead != '-' ) {
        nextch;
        ts.append( xPattern );
        xSpaceOpt;
      }
      tree = mk.makeXMLseqPat( pos, ts.toArray() );
    }
    mode = oldMode;
    //Console.println("out of xLiteralPattern, parsed:"+tree.toString());
    s.xSync2;
    tree
  }

  def xScalaExpr:Tree = {
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

  var ch: Char = _;

  /** this method assign the next character to ch and advances in input */
  def nextch: Unit = { s.xNext; ch = s.ch; }

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
  */
  def xAttributes = {
    var aMap = new ListMap[String, Tree];
    while( xml.Parsing.isNameStart( ch )) {
      val key = xName;
      xEQ;
      val delim = ch;
      val value:Tree = ch match {
        case '"' | '\'' =>
          val pos = s.pos;
          nextch;
          val tmp = xAttributeValue( delim );
          nextch;
          gen.mkStringLit( pos, tmp )
        case '{' =>
          nextch;
          xScalaExpr;
        case _ =>
	  xSyntaxError( "' or \" delimited attribute value or '{' scala-expr '}' expected" );
          gen.mkStringLit( s.pos, "<syntax-error>" )
      };
      // well-formedness constraint: unique attribute names
      if( aMap.contains( key ))
        xSyntaxError( "attribute "+key+" may only be defined once" );
      aMap = aMap.update( key, value );
      if(( ch != '/' )&&( ch != '>' ))
        xSpace;
    };
   aMap
  }


  /** attribute value, terminated by either ' or ". value may not contain <.
   *  @param endch either ' or "
   */
  def xAttributeValue( endch:char ):String = {
    while ( ch != endch ) {
      putChar( ch );
      nextch;
    };
    val str = cbuf.toString();
    cbuf.setLength( 0 );
    // @todo: normalize attribute value
    // well-formedness constraint
    if( str.indexOf('<') != -1 ) {
      xSyntaxError( "'<' not allowed in attrib value" ); ""
    } else {
      str
    }
  }

  /** parse a start or empty tag.
   *  [40] STag         ::= '<' Name { S Attribute } [S]
   *  [44] EmptyElemTag ::= '<' Name { S Attribute } [S]
   */
  def xTag = {
    val elemName = xName;
    xSpaceOpt;
    val aMap = if(xml.Parsing.isNameStart( ch )) {
      xAttributes;
    } else {
      ListMap.Empty[String,Tree];
    }
    Tuple2( elemName, aMap );
  }

  /** munch expected XML token, report syntax error for unexpected
  */
  def xToken(that: Char): Unit = {
    if( ch == that )
      nextch;
    else
      xSyntaxError("'" + that + "' expected instead of '" + ch + "'");
  }

  /* [42]  '<' xmlEndTag ::=  '<' '/' Name S? '>'                 */
  def xEndTag(n: String) = {
    xToken('/');
    val m = xName;
    if(n != m) xSyntaxError( "expected closing tag of " + n/* +", not "+m*/);
    xSpaceOpt;
    xToken('>')
  }

  def xSyntaxError(str:String) = {
    throw new RuntimeException();
    /* //DEBUG
    s.syntaxError("in XML literal: "+str);
    nextch;
    */
  }

  def sync: Unit = {
    xScalaBlock = false;
    s.xSync;
  }

  /* move forward one char
   *
   * @return true if next character  starts a scala block
   */
  def xxNext:boolean = {
    nextch;
    xCheckScalaBlock
  }

  var xScalaBlock = false;

  /** checks whether next character starts a Scala block, if yes, skip it.
   * @return true if next character starts a scala block
   */
  def xCheckScalaBlock:Boolean = {
    xScalaBlock = ( ch == '{' ) && { nextch;( ch != '{' ) };
    return xScalaBlock;
  }

  /** '<' xExpr ::= xmlTag1 '>'  { xmlExpr | '{' simpleExpr '}' } ETag
   *               | xmlTag1 '/' '>'
   */
  def xExpr:Tree = {
    var pos = s.pos;
    val Tuple2(qname: String, attrMap: ListMap[String,Tree]) = xTag;
    if(ch == '/') { // empty element
      xToken('/');
      xToken('>');
      mk.makeXML( pos, qname, attrMap, Tree.EMPTY_ARRAY );
    } else { // handle content
      xToken('>');
      val ts = new myTreeList();
      var exit = false;
      while( !exit ) {
        if( xScalaBlock ) {
          ts.append( xScalaExpr );
        } else {
          pos = s.pos;
          ch match {
            case '<' => // another tag
              nextch;
              ch match {
                case '/' => exit = true;            // end tag
                case '!' =>
                  nextch;
                  if( '[' == ch )                 // CDATA
                    ts.append( mk.CharData( pos, xCharData ));
                  else                              // comment
                    ts.append( mk.Comment( pos, xComment ));
                case '?' =>                         // PI
                  nextch;
                  ts.append( mk.ProcInstr( pos, xProcInstr ));
                case _   => ts.append( xExpr );     // child
              }

            case '{' =>
              if( xCheckScalaBlock ) {
                ts.append( xScalaExpr );
              } else {
                val str = new StringBuffer("{");
                str.append( xText );
                mk.appendTrimmed( pos, mode, ts, str.toString() )
              }
            // postcond: xScalaBlock == false!
            case '&' => // EntityRef or CharRef
              nextch;
              ch match {
                case '#' => // CharacterRef
                  nextch;
                  val theChar = mk.makeText( s.pos, false, xCharRef );
                  xToken(';');
                  ts.append( theChar );
                case _ => // EntityRef
                  val n = xName ;
                  xToken(';');
                  ts.append( mk.EntityRef( pos, n ));
              }
            case _ => // text content
              mk.appendTrimmed( pos, mode, ts, xText );
            // here xScalaBlock might be true

          }
        }
      }
      xEndTag( qname );
      mk.makeXML( pos, qname, attrMap, ts.toArray() );
    }
  }


  /** '<' xPattern  ::= Name [S] { xmlPattern | '{' pattern3 '}' } ETag
   *                  | Name [S] '/' '>'
   */
  def xPattern:Tree = {
    //Console.println("xPattern");
    val pos = s.pos;
    val qname = xName;
    xSpaceOpt;
    if( ch == '/' ) { // empty tag
      nextch;
      xToken('>');
      return mk.makeXMLpat( pos, qname, Tree.EMPTY_ARRAY );
    };

     // else: tag with content
    xToken('>');
    val ts = new myTreeList();
    var exit = false;
    while( !exit ) {
      if( xScalaBlock ) {
        ts.append( xScalaPatterns );
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
              ts.append( xScalaPatterns );
            }
            // postcond: xScalaBlock = false;
          if( xScalaBlock ) throw new ApplicationError(); // assert
          case _ => // text
            mk.appendTrimmed( pos, mode, ts, xText );
          // here  xScalaBlock might be true;
          //if( xScalaBlock ) throw new ApplicationError("after:"+text); // assert
	}
    }
    xEndTag( qname );
    mk.makeXMLpat( pos, qname, ts.toArray() );
  }

  /** '<! CharData ::= [CDATA[ ( {char} - {char}"]]>"{char} ) ']]>'
   *
   * see [15]
   */
  def xCharData:scala.xml.CharData = {
    xToken('[');
    xToken('C');
    xToken('D');
    xToken('A');
    xToken('T');
    xToken('A');
    xToken('[');
    val sb:StringBuffer = new StringBuffer();
    while (true) {
      if( ch==']'  &&
         { sb.append( ch ); nextch; ch == ']' } &&
         { sb.append( ch ); nextch; ch == '>' } ) {
        sb.setLength( sb.length() - 2 );
        nextch;
        return scala.xml.CharData( sb.toString() );
      } else sb.append( ch );
      nextch;
    }
    return null; // this cannot happen;
  };

  /** CharRef ::= "&#" '0'..'9' {'0'..'9'} ";"
   *            | "&#x" '0'..'9'|'A'..'F'|'a'..'f' { hexdigit } ";"
   *
   * see [66]
   */
  def xCharRef:String = {
    val hex  = ( ch == 'x' ) && { nextch; true };
    val base = if (hex) 16 else 10;
    var i = 0;
    while( ch != ';' ) {
      ch match {
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          i = i * base + Character.digit( ch, base );
        case 'a' | 'b' | 'c' | 'd' | 'e' | 'f'
           | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' =>
          if( !hex )
            xSyntaxError("hex char not allowed in decimal char ref\n"
                         +"Did you mean to write &#x ?");
          else
            i = i * base + Character.digit( ch, base );
        case _ =>
          xSyntaxError("character '"+ch+" not allowed in char ref\n");
      }
      nextch;
    }
    new String( Predef.Array[char]( i.asInstanceOf[char] ))
  }

  /** Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
   *
   * see [15]
   */
  def xComment:scala.xml.Comment = {
    val sb:StringBuffer = new StringBuffer();
    xToken('-');
    xToken('-');
    while (true) {
      if( ch=='-'  && { sb.append( ch ); nextch; ch == '-' } ) {
        sb.setLength( sb.length() - 1 );
        nextch;
        xToken('>');
        return scala.xml.Comment( sb.toString() );
      } else sb.append( ch );
      nextch;
    }
    return null; // this cannot happen;
  };


  /** Name ::= (Letter | '_' | ':') (NameChar)*
   *
   *  see  [5] of XML 1.0 specification
   */
  def xName: String = {
    if( xml.Parsing.isNameStart( ch ) ) {
      do {
        putChar( ch );
        nextch;
      } while( xml.Parsing.isNameChar( ch ) );
      val n = cbuf.toString().intern();
      cbuf.setLength( 0 );
      n
    } else {
      xSyntaxError( "name expected" );
      new String();
    }
  }


  /** scan [S] '=' [S]*/
  def xEQ = { xSpaceOpt; xToken('='); xSpaceOpt }

  /** skip optional space S? */
  def xSpaceOpt = { while( xml.Parsing.isSpace( ch ) ) { nextch; }}

  /** scan [3] S ::= (#x20 | #x9 | #xD | #xA)+ */
  def xSpace = {
    if( xml.Parsing.isSpace( ch ) ) {
      nextch; xSpaceOpt
    } else {
      xSyntaxError("whitespace expected");
    }
  }

  /** '<?' ProcInstr ::= Name [S ({Char} - ({Char}'>?' {Char})]'?>'
   *
   * see [15]
   */
  def xProcInstr:scala.xml.ProcInstr = {
    val sb:StringBuffer = new StringBuffer();
    val n = xName;
    if( xml.Parsing.isSpace( ch ) ) {
      xSpace;
      while( true ) {
        if( ch=='?' && { sb.append( ch ); nextch; ch == '>' } ) {
          sb.setLength( sb.length() - 1 );
          nextch;
          return scala.xml.ProcInstr( n.toString(), Some(sb.toString()) );
        } else
          sb.append( ch );
        nextch;
      }
    };
    xToken('?');
    xToken('>');
   scala.xml.ProcInstr( n.toString(), None );
  }

  /** parse character data.
  *   precondition: xScalaBlock == false (we are not in a scala block)
  */
  def xText:String = {
    if( xScalaBlock ) throw new ApplicationError(); // assert

    if( xCheckScalaBlock )
      return ""
    else {
      var exit = false;
      while( !exit ) {
        putChar( ch );
        exit = xxNext || ( ch == '<' ) || ( ch == '&' );
      }
      val str = cbuf.toString();
      cbuf.setLength( 0 );
      str
    }
  }


} /* class MarkupParser */
}
