package scala.xml.parsing ;

import scala.collection.mutable;
import scala.collection.immutable.ListMap;

abstract class MarkupParser[MarkupType, AVType] {

  val handle: MarkupHandler[MarkupType, AVType];

  /** character buffer, for names */
  protected val cbuf = new StringBuffer();

  /** append Unicode character to name buffer*/
  protected def putChar(c: char) = cbuf.append( c );

  /** holds the next character */
  var ch: Char = _;

  /** holds the position in the source file */
  var pos: Int = _;

  /** whether to remove surplus whitespace or not */
  val preserveWS: Boolean;

  var xEmbeddedBlock = false;

  /** this method assign the next character to ch and advances in input */
  def nextch: Unit;

  /** this method should assign the first character of the input to ch */
  def init: Unit;

  /** report a syntax error */
  def xSyntaxError(str:String): Unit;

  /** munch expected XML token, report syntax error for unexpected
  */
  def xToken(that: Char): Unit = {
    if( ch == that )
      nextch;
    else
      xSyntaxError("'" + that + "' expected instead of '" + ch + "'");
  }

  /** checks whether next character starts a Scala block, if yes, skip it.
   * @return true if next character starts a scala block
   */
  def xCheckEmbeddedBlock:Boolean = {
    xEmbeddedBlock = ( ch == '{' ) && { nextch;( ch != '{' ) };
    return xEmbeddedBlock;
  }

  /** parse attribute and add it to listmap
   *  [41] Attributes    ::= { S Name Eq AttValue }
   *       AttValue     ::= `'` { _  } `'`
   *                      | `"` { _ } `"`
   *                      | `{` scalablock `}`
  */
  def xAttributes = {
    var aMap = new mutable.HashMap[String, AttribValue[AVType]];
    while( xml.Parsing.isNameStart( ch )) {
      val key = xName;
      xEQ;
      val delim = ch;
      val pos1 = pos;
      val value:AttribValue[AVType] = ch match {
        case '"' | '\'' =>
          nextch;
          val tmp = xAttributeValue( delim );
          nextch;
          handle.attributeCDataValue( pos1, tmp );
        case '{' =>
          nextch;
          handle.attributeEmbedded(pos1, xEmbeddedExpr);
        case _ =>
	  xSyntaxError( "' or \" delimited attribute value or '{' scala-expr '}' expected" );
          handle.attributeCDataValue( pos1, "<syntax-error>" )
      };
      // well-formedness constraint: unique attribute names
      if( aMap.contains( key ))
        xSyntaxError( "attribute "+key+" may only be defined once" );
      aMap.update( key, value );
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
  def xTag: Pair[String, mutable.Map[String,AttribValue[AVType]]] = {
    val elemName = xName;
    xSpaceOpt;
    val aMap = if(xml.Parsing.isNameStart( ch )) {
      xAttributes;
    } else {
      new mutable.HashMap[String,AttribValue[AVType]]();
    }
    Tuple2( elemName, aMap );
  }

  /* [42]  '<' xmlEndTag ::=  '<' '/' Name S? '>'                 */
  def xEndTag(n: String) = {
    xToken('/');
    val m = xName;
    if(n != m) xSyntaxError( "expected closing tag of " + n/* +", not "+m*/);
    xSpaceOpt;
    xToken('>')
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

  def appendText(pos: int, ts:mutable.Buffer[MarkupType], txt:String):Unit = {
    if( !preserveWS )
      for( val t <- TextBuffer.fromString( txt ).toText ) {
        ts.append( handle.text( pos, t.text ) );
      }
    else
      ts.append( handle.text( pos, txt ));
  }


  def content: mutable.Buffer[MarkupType] = {
    var ts = new mutable.ArrayBuffer[MarkupType];
    var exit = false;
    while( !exit ) {
      if( xEmbeddedBlock ) {
        ts.append( xEmbeddedExpr );
      } else {
        val pos2 = pos;
        ch match {
          case '<' => // another tag
            nextch;
            ch match {
              case '/' => exit = true;            // end tag
              case '!' =>
                nextch;
                if( '[' == ch )                 // CDATA
                  ts.append(handle.charData( pos2, xCharData ));
                else                              // comment
                  ts.append(handle.comment( pos2, xComment ));
              case '?' =>                         // PI
                nextch;
                ts.append(handle.procInstr( pos2, xProcInstr ));
              case _   =>
                ts.append( element );     // child
            }

          case '{' =>
            if( xCheckEmbeddedBlock ) {
              ts.append(xEmbeddedExpr);
            } else {
              val str = new StringBuffer("{");
              str.append( xText );
              appendText(pos2, ts, str.toString());
              //@todo
            }
          // postcond: xEmbeddedBlock == false!
          case '&' => // EntityRef or CharRef
            nextch;
            ch match {
              case '#' => // CharacterRef
                nextch;
              val theChar = handle.text( pos2, xCharRef );
              xToken(';');
              ts.append( theChar );
              case _ => // EntityRef
                val n = xName ;
                xToken(';');
                ts.append( handle.entityRef( pos2, n ) );
            }
          case _ => // text content
            appendText(pos2, ts, xText);
          // here xEmbeddedBlock might be true
        }
      }
    }
    ts
  } /* end content */

  /** '<' element ::= xmlTag1 '>'  { xmlExpr | '{' simpleExpr '}' } ETag
   *               | xmlTag1 '/' '>'
   */
  def element: MarkupType = {
    var pos1 = pos;
    val Tuple2(qname, attrMap) = xTag;
    if(ch == '/') { // empty element
      xToken('/');
      xToken('>');
      handle.element( pos1, qname, attrMap, new mutable.ListBuffer[MarkupType] );
    } else { // handle content
      xToken('>');
      val ts = content;
      xEndTag( qname );
      handle.element( pos1, qname, attrMap, ts );
    }
  }

  def xEmbeddedExpr: MarkupType;

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
  *   precondition: xEmbeddedBlock == false (we are not in a scala block)
  */
  def xText:String = {
    if( xEmbeddedBlock ) throw FatalError("internal error: encountered embedded block"); // assert

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

}
