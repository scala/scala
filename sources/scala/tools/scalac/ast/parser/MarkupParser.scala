
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
    var symt = scalaDot(s.pos, Names.Symbol);
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

  def makeXML(pos:int,n:Name,args:Array[Tree],attrMap:ListMap[Name,String]):Tree = {
    var t = makeXML( pos, n, args );
    if( attrMap.isEmpty ) {
      t
    } else {
      val attrs = new Array[Tree]( attrMap.size );
      val it = attrMap.elements ;
      var i = 0; while( i < attrs.length ) {
	val Pair( key, value ) = it.next;
	attrs( i ) = make.Apply(pos, scalaDot(s.pos, Names.Tuple2), {
	  val x = new Array[Tree](2);
          x( 0 ) = gen.mkStringLit( pos, key.toString() );
          x( 1 ) = gen.mkStringLit( pos, value.toString() );
          x });
	i = i + 1;
      };
      make.Apply(pos, make.Select( pos, t, Names.PERCENT ), {
	val x = new Array[Tree](1);
	x( 0 ) = make.Apply(pos,
                            scalaDot(s.pos, Names.List),
                            attrs);
	x })
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

  /** parse a start or empty tag.
   *  [40] STag         ::= '<' Name { S Attribute } [S]
   *  [41] Attribute    ::= Name Eq AttValue
   *  [44] EmptyElemTag ::= '<' Name { S Attribute } [S]
   */
  def xTag:Tuple2[Name, ListMap[Name,String]] = {
    val elemName = s.xName;
    s.xSpaceOpt;
    var attrMap = ListMap.Empty[Name,String];
    while( s.xIsNameStart ) {
      val attrName = s.xName;
      s.xEQ;
      val endch = s.ch.asInstanceOf[char];
      val attrValue = endch match {
        case '"' | '\'' =>
          s.xNext; val tmp = s.xAttributeValue( endch );
	  s.xNext; s.xSpaceOpt; tmp
        case _ =>
	  s.xSyntaxError( "' or \" delimited attribute value expected" );
	""
      }
      // well-formedness constraint: unique attribute names
      if( attrMap.contains( attrName ))
        s.xSyntaxError( "attribute "+attrName+" may only be defined once" );

      attrMap = attrMap.update( attrName, attrValue );
    }
    Tuple2( elemName, attrMap );
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
      s.xToken('/'); s.xToken('>'); makeXML( pos, elemName, Tree.EMPTY_ARRAY );
    } else { // handle content
      s.xToken('>'); s.xSpaceOpt;

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
      makeXML( pos, elemName, ts.toArray(), attrMap );
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
