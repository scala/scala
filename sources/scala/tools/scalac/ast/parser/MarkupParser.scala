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

package scala.tools.scalac.ast.parser {

class MarkupParser(unit: CompilationUnit, s: Scanner, p: Parser, presWS: boolean ) /*with scala.xml.parsing.MarkupParser[Tree,Tree] */{

  import Tokens.{EMPTY, LBRACE, RBRACE} ;
  import scala.tools.scalac.ast.{TreeList => myTreeList}

  final val preserveWS = presWS;

  /** the XML tree factory */
  val handle: SymbolicXMLBuilder = new SymbolicXMLBuilder( unit.global.make, unit.global.treeGen, p, presWS );

  /** holds the position in the source file */
  /*[Duplicate]*/ var pos: Int = _;

  /** holds temporary values of pos */
  /*[Duplicate]*/ var tmppos: Int = _;

  /** holds the next character */
  /*[Duplicate]*/   var ch: Char = _;

  /** character buffer, for names */
  /*[Duplicate]*/   protected val cbuf = new StringBuffer();

  /** append Unicode character to name buffer*/
  /*[Duplicate]*/   protected def putChar(c: char) = cbuf.append( c );

  /*[Duplicate]*/ var xEmbeddedBlock = false;

  /** munch expected XML token, report syntax error for unexpected */
  /*[Duplicate]*/ def xToken(that: Char): Unit = {
    if( ch == that )
      nextch;
    else
      reportSyntaxError("'" + that + "' expected instead of '" + ch + "'");
  }

  var debugLastStartElement = new mutable.Stack[Pair[Int,String]];

  /** checks whether next character starts a Scala block, if yes, skip it.
   * @return true if next character starts a scala block
   */
  /*[Duplicate]*/ def xCheckEmbeddedBlock:Boolean = {
    xEmbeddedBlock =
      enableEmbeddedExpressions && ( ch == '{' ) && { nextch;( ch != '{' ) };
    return xEmbeddedBlock;
  }

  /** parse attribute and add it to listmap
   *  [41] Attributes    ::= { S Name Eq AttValue }
   *       AttValue     ::= `'` { _  } `'`
   *                      | `"` { _ } `"`
   *                      | `{` scalablock `}`
  */
  /*[Duplicate]*/ def xAttributes = {
    var aMap = new mutable.HashMap[String, Tree]();
    while( xml.Parsing.isNameStart( ch )) {
      val key = xName;
      xEQ;
      val delim = ch;
      val pos1 = pos;
      val value: /* AttribValue[*/Tree/*]*/ = ch match {
        case '"' | '\'' =>
          nextch;
          val tmp = xAttributeValue( delim );
          nextch;
          gen.mkStringLit(pos1, tmp);
        case '{' if enableEmbeddedExpressions =>
          nextch;
          xEmbeddedExpr;
        case _ =>
	  reportSyntaxError( "' or \" delimited attribute value or '{' scala-expr '}' expected" );
          gen.mkStringLit(pos1, "<syntax-error>" )
      };
      // well-formedness constraint: unique attribute names
      if( aMap.contains( key ))
        reportSyntaxError( "attribute "+key+" may only be defined once" );
      aMap.update( key, value );
      if(( ch != '/' )&&( ch != '>' ))
        xSpace;
    };
   aMap
  }

  /** attribute value, terminated by either ' or ". value may not contain <.
   *  @param endch either ' or "
   */
  /*[Duplicate]*/ def xAttributeValue( endch:char ):String = {
    while ( ch != endch ) {
      putChar( ch );
      nextch;
    };
    val str = cbuf.toString();
    cbuf.setLength( 0 );
    // @todo: normalize attribute value
    // well-formedness constraint
    if( str.indexOf('<') != -1 ) {
      reportSyntaxError( "'<' not allowed in attrib value" ); ""
    } else {
      str
    }
  }

 /** parse a start or empty tag.
   *  [40] STag         ::= '<' Name { S Attribute } [S]
   *  [44] EmptyElemTag ::= '<' Name { S Attribute } [S]
   */
  /*[Duplicate]*/ def xTag: Pair[String, mutable.Map[String, Tree]] = {
    val elemName = xName;
    xSpaceOpt;
    val aMap = if(xml.Parsing.isNameStart( ch )) {
      xAttributes;
    } else {
      new mutable.HashMap[String, Tree]();
    }
    Tuple2( elemName, aMap );
  }

  /* [42]  '<' xmlEndTag ::=  '<' '/' Name S? '>'                 */
  /*[Duplicate]*/ def xEndTag(n: String) = {
    xToken('/');
    val m = xName;
    if(n != m) reportSyntaxError( "expected closing tag of " + n/* +", not "+m*/);
    xSpaceOpt;
    xToken('>')
  }

 /** '<! CharData ::= [CDATA[ ( {char} - {char}"]]>"{char} ) ']]>'
   *
   * see [15]
   */
  /*[Duplicate]*/ def xCharData: Tree = {
    xToken('[');
    xToken('C');
    xToken('D');
    xToken('A');
    xToken('T');
    xToken('A');
    xToken('[');
    val pos1 = pos;
    val sb:StringBuffer = new StringBuffer();
    while (true) {
      if( ch==']'  &&
         { sb.append( ch ); nextch; ch == ']' } &&
         { sb.append( ch ); nextch; ch == '>' } ) {
        sb.setLength( sb.length() - 2 );
        nextch;
        return handle.charData( pos1, sb.toString() );
      } else sb.append( ch );
      nextch;
    }
    throw new ApplicationError("this cannot happen");
  };

  /** CharRef ::= "&#" '0'..'9' {'0'..'9'} ";"
   *            | "&#x" '0'..'9'|'A'..'F'|'a'..'f' { hexdigit } ";"
   *
   * see [66]
   */
  /*[Duplicate]*/ def xCharRef:String = {
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
            reportSyntaxError("hex char not allowed in decimal char ref\n"
                         +"Did you mean to write &#x ?");
          else
            i = i * base + Character.digit( ch, base );
        case _ =>
          reportSyntaxError("character '"+ch+" not allowed in char ref\n");
      }
      nextch;
    }
    new String( Predef.Array[char]( i.asInstanceOf[char] ))
  }
/** Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
   *
   * see [15]
   */
  /*[Duplicate]*/ def xComment: Tree = {
    val sb:StringBuffer = new StringBuffer();
    xToken('-');
    xToken('-');
    while (true) {
      if( ch=='-'  && { sb.append( ch ); nextch; ch == '-' } ) {
        sb.setLength( sb.length() - 1 );
        nextch;
        xToken('>');
        return handle.comment( pos, sb.toString() );
      } else sb.append( ch );
      nextch;
    }
    throw new ApplicationError("this cannot happen");
  };

  /*[Duplicate]*/ def appendText(pos: int, ts:mutable.Buffer[Tree], txt:String):Unit = {
    if( !preserveWS )
      for( val t <- TextBuffer.fromString( txt ).toText ) {
        ts.append( handle.text( pos, t.text ) );
      }
    else
      ts.append( handle.text( pos, txt ));
  }

  /*[Duplicate]*/ def content: mutable.Buffer[Tree] = {
    var ts = new mutable.ArrayBuffer[Tree];
    var exit = false;
    while( !exit ) {
      if( xEmbeddedBlock ) {
        ts.append( xEmbeddedExpr );
      } else {
        tmppos = pos;
        ch match {
          case '<' => // another tag
            nextch;
            ch match {
              case '/' =>
                exit = true;                    // end tag
              case '!' =>
                nextch;
                if( '[' == ch )                 // CDATA
                  ts.append( xCharData );
                else                            // comment
                  ts.append( xComment );
              case '?' =>                       // PI
                nextch;
                ts.append( xProcInstr );
              case _   =>
                ts.append( element );     // child
            }

          case '{' =>
            if( xCheckEmbeddedBlock ) {
              ts.append(xEmbeddedExpr);
            } else {
              val str = new StringBuffer("{");
              str.append( xText );
              appendText(tmppos, ts, str.toString());
            }
          // postcond: xEmbeddedBlock == false!
          case '&' => // EntityRef or CharRef
            nextch;
            ch match {
              case '#' => // CharacterRef
                nextch;
              val theChar = handle.text( tmppos, xCharRef );
              xToken(';');
              ts.append( theChar );
              case _ => // EntityRef
                val n = xName ;
                xToken(';');
                ts.append( handle.entityRef( tmppos, n ) );
            }
          case _ => // text content
            appendText(tmppos, ts, xText);
          // here xEmbeddedBlock might be true
        }
      }
    }
    ts
  } /* end content */

  /** '<' element ::= xmlTag1 '>'  { xmlExpr | '{' simpleExpr '}' } ETag
   *               | xmlTag1 '/' '>'
   */
  /*[Duplicate]*/ def element: Tree = {
    val pos1 = pos;
    val Tuple2(qname, attrMap) = xTag;
    if(ch == '/') { // empty element
      xToken('/');
      xToken('>');
      handle.element( pos1, qname, attrMap, new mutable.ListBuffer[Tree] );
    } else { // handle content
      xToken('>');
      debugLastStartElement.push(Pair(pos1,qname));
      val ts = content;
      xEndTag( qname );
      debugLastStartElement.pop;
      handle.element( pos1, qname, attrMap, ts );
    }
  }


  /** Name ::= (Letter | '_' | ':') (NameChar)*
   *
   *  see  [5] of XML 1.0 specification
   */
  /*[Duplicate]*/   def xName: String = {
    if( xml.Parsing.isNameStart( ch ) ) {
      do {
        putChar( ch );
        nextch;
      } while( xml.Parsing.isNameChar( ch ) );
      val n = cbuf.toString().intern();
      cbuf.setLength( 0 );
      n
    } else {
      reportSyntaxError( "name expected" );
      new String();
    }
  }


  /** scan [S] '=' [S]*/
  /*[Duplicate]*/   def xEQ = { xSpaceOpt; xToken('='); xSpaceOpt }

  /** skip optional space S? */
  /*[Duplicate]*/   def xSpaceOpt = { while( xml.Parsing.isSpace( ch ) ) { nextch; }}

  /** scan [3] S ::= (#x20 | #x9 | #xD | #xA)+ */
  /*[Duplicate]*/ def xSpace = {
    if( xml.Parsing.isSpace( ch ) ) {
      nextch; xSpaceOpt
    } else {
      reportSyntaxError("whitespace expected");
    }
  }

/** '<?' ProcInstr ::= Name [S ({Char} - ({Char}'>?' {Char})]'?>'
   *
   * see [15]
   */
  /*[Duplicate]*/   def xProcInstr: Tree = {
    val sb:StringBuffer = new StringBuffer();
    val n = xName;
    if( xml.Parsing.isSpace( ch ) ) {
      xSpace;
      while( true ) {
        if( ch=='?' && { sb.append( ch ); nextch; ch == '>' } ) {
          sb.setLength( sb.length() - 1 );
          nextch;
          return handle.procInstr(tmppos, n.toString(), sb.toString());
        } else
          sb.append( ch );
        nextch;
      }
    };
    xToken('?');
    xToken('>');
    return handle.procInstr(tmppos, n.toString(), sb.toString());
  }

  /** parse character data.
  *   precondition: xEmbeddedBlock == false (we are not in a scala block)
  */
  /*[Duplicate]*/ def xText: String = {
    if( xEmbeddedBlock ) throw new ApplicationError("internal error: encountered embedded block"); // assert

    if( xCheckEmbeddedBlock )
      return ""
    else {
      var exit = false;
      while( !exit ) {
        putChar( ch );
        exit = { nextch; xCheckEmbeddedBlock }||( ch == '<' ) || ( ch == '&' );
      }
      val str = cbuf.toString();
      cbuf.setLength( 0 );
      str
    }
  }
  //override type Tree = handle.Tree;
  //override type Tree     = handle.Tree;

  /** the XML tree builder */
  val gen = unit.global.treeGen ;

  final val PATTERN = true;
  final val EXPR    = false;

  val enableEmbeddedExpressions: Boolean = true;

  //val cbuf = new StringBuffer();

  /** append Unicode character to name buffer*/
  //private def putChar(c: char) = cbuf.append( c );

  /** xLiteral = element { element }
   * @return Scala representation of this xml literal
   * precondition: s.xStartsXML == true
  */
  def xLiteral: Tree = try {
    init;
    handle.isPattern = false;
    val pos = s.pos;
    var tree = element;
    xSpaceOpt;
    // parse more XML ?
    if( ch=='<' )  {
      val ts = new mutable.ArrayBuffer[Tree]();
      ts.append( tree );
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
  } catch {
    case _:ArrayIndexOutOfBoundsException =>
      s.syntaxError(debugLastStartElement.top._1,
                    "missing end tag in XML literal for <"
                    +debugLastStartElement.top._2+">");
      Tree.Empty;
  }

  /** @see xmlPattern. resynchronizes after succesful parse
   * @return this xml pattern
   * precondition: s.xStartsXML == true
  */
  def xLiteralPattern:Tree = try {
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
  }catch {
    case _:ArrayIndexOutOfBoundsException =>
      s.syntaxError(debugLastStartElement.top._1,
                    "missing end tag in XML literal for <"
                    +debugLastStartElement.top._2+">");
      Tree.Empty;
  }

  def xEmbeddedExpr:Tree = {
    sync;
    val b = p.expr(true,false);
    if(s.token != RBRACE)
      reportSyntaxError(" expected end of Scala block");
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
      reportSyntaxError(" expected end of Scala patterns");
    init;
    return b
  }

  //var ch: Char = _;

  /** this method assign the next character to ch and advances in input */
  def nextch: Unit = { s.xNext; ch = s.ch; pos = s.pos; }

  def lookahead = { s.xLookahead }

  def init: Unit = {
    ch = s.ch;
    pos = s.pos;
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


  def reportSyntaxError(str:String) = {
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
    debugLastStartElement.push(Pair(pos1,qname));
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
    debugLastStartElement.pop;
    handle.makeXMLpat( pos1, qname, ts );
  }

} /* class MarkupParser */
}
