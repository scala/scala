
import scalac.ast._;
import scalac.atree.AConstant;
import scalac._;
import scalac.util._;
import ch.epfl.lamp.util.Position;
import java.util.{Map, Stack, ArrayList, LinkedList};
import java.lang.{Integer, Long, Float, Double};
import scala.Iterator;
import scala.tools.scalac.util.NewArray;
import scala.collection.immutable.ListMap ;
import scala.collection.mutable.Buffer;

package scala.tools.scalac.ast.parser {

class MarkupParser( unit:Unit, s:Scanner, p:Parser ) {

  import Tokens.{EMPTY, RBRACE} ;
  import scala.tools.scalac.ast.{TreeList => myTreeList}

  /** the tree factory
   */
  val make: TreeFactory = unit.global.make;

  /** the tree generator
  */
  val gen: TreeGen = unit.global.treeGen;

  /** convenience method */
  def scalaDot(pos: int, name: Name): Tree =
    make.Select(pos, make.Ident(pos, Names.scala), name);

  /** convenience method */
  def convertToTypeId(t: Tree): Tree = t match {
    case Tree$Ident(name) =>
      make.Ident(t.pos, name.toTypeName())
    case Tree$Select(qual, name) =>
      make.Select(t.pos, qual, name.toTypeName())
    case _ =>
      t
  }

  // create scala xml tree

  def mkXML(pos:int, isPattern:boolean, t:Tree, args:Array[Tree]):Tree = {
    var symt = scalaDot( pos, Names.Symbol );
    if( isPattern ) symt = convertToTypeId(symt);
    val ts = new myTreeList();
    ts.append(t);
    ts.append(args);
    make.Apply(pos, symt, ts.toArray());
  }

  def makeXMLpat(pos:int, n:Name, args:Array[Tree]):Tree =
    mkXML(pos, true, gen.mkStringLit( pos, n.toString() ), args);

  def makeXML(pos:int, n:Name, args:Array[Tree]):Tree =
    mkXML(pos, false, gen.mkStringLit( pos, n.toString() ), args);

  def makeXMLseq( pos:int, args:Array[Tree] ) = {
    make.Apply(pos, scalaDot(s.pos, Names.List), args);
  }

  def makeXML(pos:int,n:Name,args:Array[Tree],attrMap:ListMap[Name,Tree]):Tree = {
    var t = makeXML( pos, n, args );
    if( attrMap.isEmpty ) {
      t
    } else {
      val attrs = new Array[Tree]( attrMap.size );
      var i = 0;
      for( val Pair( key, value ) <- attrMap.elements ) {
	attrs( i ) = make.Apply(pos,
                                scalaDot(s.pos, Names.Tuple2),
                                Predef.Array(gen.mkStringLit( pos, key.toString() ),
                                             value));
	i = i + 1;
      };
      make.Apply(pos,
                 make.Select( pos, t, Names.PERCENT ),
	         Predef.Array( make.Apply(pos, scalaDot(s.pos, Names.List),
                            attrs)));
    }
  }

  /** xLiteral = xExpr { xExpr }
   * @return Scala representation of this xml literal
   * precondition: s.xStartsXML == true
  */
  def xLiteral:Tree = {
    val pos = s.pos;
    var tree = xExpr; s.token = EMPTY; s.nextToken();
    if( s.xStartsXML )  {
      val ts = new myTreeList(); ts.append( tree );
      while( s.xStartsXML ) { ts.append( xExpr ); s.nextToken(); }
      tree = makeXMLseq( pos, ts.toArray() );
    }
    tree
  }

  /** parse attribute and add it to listmap
   *  [41] Attributes    ::= { S Name Eq AttValue }
   *       AttValue     ::= `'` { _ | `{` scalablock `}` } `'`
   *                    ::= `"` { _ | `{` scalablock `}` } `"`
  */
  def xAttributes = {
    var aMap = ListMap.Empty[Name,Tree];
    while( s.xIsNameStart ) {
      val key = s.xName; s.xEQ;
      val endch = s.ch.asInstanceOf[char];
      val value = endch match {
        case '"' | '\'' =>
          val pos = s.pos;
          s.xNext; val tmp = s.xAttributeValue( endch );
          s.xNext; s.xSpaceOpt;
          gen.mkStringLit( pos, tmp )
        case '{' =>
          s.nextToken(); // = LBRACE
          s.nextToken();
          val tmp = p.expr(false,false);
          if( s.token != RBRACE ) {
            s.xSyntaxError("expected end of Scala block");
          };
          tmp
        case _ =>
	  s.xSyntaxError( "' or \" delimited attribute value or '{' scala-expr '}' expected" );
          make.Bad(s.pos)
      }
      // well-formedness constraint: unique attribute names
      if( aMap.contains( key ))
        s.xSyntaxError( "attribute "+key+" may only be defined once" );
      aMap = aMap.update( key, value );
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
    val aMap = if( s.xIsNameStart ) {
      xAttributes;
    } else {
      ListMap.Empty[Name,Tree];
    }
    Tuple2( elemName, aMap );
  }

  /* [42]  '<' xmlEndTag ::=  '<' '/' Name S? '>'                 */
  def xEndTag( n:Name ) = {
    s.xToken('/');
    if( n != s.xName ) s.xSyntaxError( "expected closing tag of " + n );
    s.xSpaceOpt;
    s.xToken('>')
  }

  /** '<' xExpr ::= xmlTag1 '>'  { xmlExpr | '{' simpleExpr '}' } ETag
   *               | xmlTag1 '/' '>'
   *  the caller has to resynchronize with s.token = EMPTY; s.nextToken;
   */
  def xExpr:Tree = {
    val pos = s.pos;
    val Tuple2( elemName, attrMap ) = xTag;
    if( s.ch == '/' ) { // empty element
      s.xToken('/'); s.xToken('>');
      makeXML( pos, elemName, Tree.EMPTY_ARRAY, attrMap );
    } else { // handle content
      s.xToken('>');
      val ts = new myTreeList();
      var exit = false;
      while( !exit ) {
        /* Console.println("read '"+s.ch.asInstanceOf[char]+"'"); */
        s.ch match {
          case '<' => // another tag
            s.xNext; s.ch match {
              case '/' => exit = true;            // end tag
              case '!' => s.xComment;
              case _   => ts.append( xExpr ); // parse child
            }
          case '{' => // Scala block(s)
            while( s.ch == '{' ) {
              s.nextToken();
              s.nextToken();
              val bs = new myTreeList();
              val b = p.expr(true,false); //block( s.pos );
              if( s.token != RBRACE ) {
                s.xSyntaxError(" expected end of Scala block");
              }
              ts.append( b );
            }
          case _ => // text content
            ts.append( gen.mkStringLit( s.pos, s.xText ));
        }
      }
      xEndTag( elemName );
      val t2 = makeXML( pos, elemName, ts.toArray(), attrMap );
        Console.println("parsed:"+t2);
        t2
    }
  }

  /** @see xmlPattern. resynchronizes after succesful parse */
  def xLiteralPattern = {
    val t = xPattern; s.nextToken(); t
  }

  /** '<' xPattern  ::= Name [S] { xmlPattern | '{' pattern3 '}' } ETag
   *                  | Name [S] '/' '>'
   */
  def xPattern:Tree = {
    val pos = s.pos;
    val elemName = s.xName;
    s.xSpaceOpt;
    if( s.ch == '/' ) { // empty tag
      s.xNext; s.xToken('>'); makeXMLpat( pos, elemName, Tree.EMPTY_ARRAY );
    } else { // content
      s.xToken('>');
      val ts = new myTreeList();
      var exit = false;
      while( !exit ) {
        //Console.print("["+s.ch.asInstanceOf[char]+"]");
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
              s.nextToken();
              s.nextToken();
              val ps = p.patterns();
              if( s.token != RBRACE ) {
                s.xSyntaxError(" expected end of Scala block");
              }
              ts.append( ps );
            }
          case _ => // text
            val pos = s.pos;
            ts.append( gen.mkStringLit( pos, s.xText ) );
	}
      }
      xEndTag( elemName );
      makeXMLpat( pos, elemName, ts.toArray() );
    }
  }

} /* class MarkupParser */
}
