/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002-2004, LAMP/EPFL         **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

import scalac.ast._;
import scalac.atree.AConstant;
import scalac._;
import scalac.util._;
import scala.tools.util.Position;
import java.lang.{Integer, Long, Float, Double};
import scala.Iterator;
import scala.tools.scalac.util.NewArray;
import scala.collection.immutable.ListMap ;
import scala.collection.mutable.Buffer;
import scalac.symtab.Modifiers;
package scala.tools.scalac.ast.parser {

class MarkupParser( unit:Unit, s:Scanner, p:Parser ) {

  import Tokens.{EMPTY, LBRACE, RBRACE} ;
  import scala.tools.scalac.ast.{TreeList => myTreeList}

  val  _AppendBuffer = Name.fromString("AppendBuffer");
  val  _collection = Name.fromString("collection");
  val  _Elem = Name.fromString("Elem");
  val  _Seq = Name.fromString("Seq");
  val  _mutable = Name.fromString("mutable");
  val  _append = Name.fromString("append");
  val  _xml = Name.fromString("xml");
  val  _Node = Name.fromString("Node");
  val  _Text = Name.fromString("Text");
  val  _EntityRef = Name.fromString("EntityRef");

  /** the tree factory
   */
  val make: TreeFactory = unit.global.make;

  /** the tree generator
  */
  val gen: TreeGen = unit.global.treeGen;

  // convenience methods
  private def _scala( pos: int, name: Name ) =
    make.Select( pos, make.Ident( pos, Names.scala ), name );

  private def _scala_Seq( pos: int ) =
    /*make.Apply( pos,
               make.AppliedType(pos,*/
                                p.convertToTypeId( _scala( pos, _Seq ))/*,
                                (Predef.Array[Tree](
                                  convertToTypeId(
                                    _scala_xml_Node( pos ) ))
                              ),
               Tree.EMPTY_ARRAY)*/;

  private def _scala_xml( pos: int, name: Name ) =
    make.Select( pos, _scala( pos, _xml ), name );

  private def _scala_xml_Node( pos: int ) =
    _scala_xml( pos, _Node );

  private def _scala_xml_Text( pos: int ) =
    p.convertToConstr( _scala_xml( pos, _Text ));

  private def _scala_xml_EntityRef( pos: int ) =
    p.convertToConstr( _scala_xml( pos, _EntityRef ));

  private def _scala_collection( pos: int, name: Name ) =
    make.Select( pos, _scala( pos, _collection ), name );

  private def _scala_collection_mutable( pos: int, name: Name ) =
    make.Select(pos, _scala_collection(pos, _mutable ), name);

  private def _scala_collection_mutable_AppendBuffer( pos: int ) =
    make.Apply( pos,
               make.AppliedType(pos,
                                p.convertToConstr(
                                  _scala_collection_mutable(pos, _AppendBuffer )),
                                Predef.Array[Tree](
                                  convertToTypeId(
                                    _scala_xml_Node( pos ) ))
                              ),
               Tree.EMPTY_ARRAY );

  private def _scala_Tuple2( pos:int ) =
    _scala( pos, Names.Tuple2 );

  private def _scala_xml_Elem( pos:int ) =
    _scala_xml( pos, _Elem );

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
    if( isPattern ) {
      val ts = new myTreeList();
      ts.append( t );
      ts.append( convertToText( args ) );
      make.Apply(pos,
                 convertToTypeId( _scala_xml_Elem( pos ) ),
                 ts.toArray())
    } else {
      val constrArgs = if( 0 == args.length ) {
        Predef.Array[Tree]( t )
      } else {
        Predef.Array[Tree]( t, make.Typed(
          pos, makeXMLseq(pos, args), make.Ident(pos, TypeNames.WILDCARD_STAR)))
      };
      make.Apply( pos, _scala_xml_Elem( pos ), constrArgs )
    }
  }

  def makeEntityRef( pos:int, n:Name ) = {
    val constr = make.Apply( pos,
                            _scala_xml_EntityRef( pos ),
                            Predef.Array[Tree]( gen.mkStringLit( pos, n.toString() )));

    make.New( pos, constr );
  };
  // create scala.xml.Text here <: scala.xml.Node
  def makeText( pos: int, txt:String ):Tree =
    makeText( pos, gen.mkStringLit( pos, txt ));

  def makeText( pos: int, txt:Tree ):Tree = {
    val constr = make.Apply( pos,
                           _scala_xml_Text( pos ),
                           Predef.Array[Tree] ( txt ));
    make.New( pos, constr );
  }

  def makeXMLpat(pos:int, n:Name, args:Array[Tree]):Tree =
    mkXML(pos, true, gen.mkStringLit( pos, n.toString() ), args);

  def makeXML(pos:int, n:Name, args:Array[Tree]):Tree =
    mkXML(pos, false, gen.mkStringLit( pos, n.toString() ), args);

  def convertToText( t:Tree ) = t match {
    case _:Tree$Literal => makeText( t.pos, t );
    case _ => t
  }

  def convertToText( ts:Array[Tree] ) = {
    var res:Array[Tree] = null;
    var i = 0; while( i < ts.length ) {
      ts( i ) match {
        case _:Tree$Literal =>
          if( null == res ) {  // lazy copy
            res = new Array[Tree]( ts.length );
            System.arraycopy( ts, 0, res, 0, i );
          }
          res( i ) = makeText( ts( i ).pos, ts( i ) );
        case _ =>
      }
      i = i + 1
    }
    if( null == res ) ts else res;
  }

  def makeXMLseq( pos:int, args:Array[Tree] ) = {
    val blocArr = new Array[Tree] ( 1 + args.length );
    val constr = _scala_collection_mutable_AppendBuffer( pos );
    val n = p.fresh();
    val nIdent = make.Ident(pos, n);
    blocArr( 0 ) = make.ValDef(pos, Modifiers.MUTABLE, n, Tree.Empty,
                               make.New( pos, constr ));

    var i = 0; while( i < args.length ) {
      val ipos = args(i).pos;
      val t = make.Apply( ipos,
                          make.Select( ipos, nIdent, _append ),
                          Predef.Array[Tree]( convertToText( args( i ) )));
      i = i + 1;
      blocArr( i ) = t
    }
    make.Block( pos, blocArr, nIdent );
  }

  def makeXMLseqPat( pos:int, args:Array[Tree] ) = {
    make.Apply( pos, _scala_Seq( pos ), args );
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
                                _scala_Tuple2( s.pos ),
                                Predef.Array(gen.mkStringLit( pos, key.toString() ),
                                             value));
	i = i + 1;
      };
      make.Apply(pos,
                 make.Select( pos, t, Names.PERCENT ),
	         Predef.Array( make.Apply(pos, _scala(s.pos, Names.List),
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
   *       AttValue     ::= `'` { _  } `'`
   *                      | `"` { _ } `"`
   *                      | `{` scalablock `}`
  */
  def xAttributes = {
    var aMap = ListMap.Empty[Name,Tree];
    while( s.xIsNameStart ) {
      val key = s.xName;
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
          s.nextToken();
          val tmp = p.expr(false,false);
          if( s.token != RBRACE ) {
            s.xSyntaxError("expected end of Scala block");
          };
          tmp
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

  def xScalaExpr( ts:myTreeList ) = {
    s.nextToken();
    s.xScalaBlock = false;
    val b = p.expr(true,false);
    if( s.token != RBRACE ) {
      s.xSyntaxError(" expected end of Scala block");
    }
    ts.append( b );
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
        if( s.xScalaBlock ) {
          xScalaExpr( ts );
        } else {
          s.ch match {

            case '<' => // another tag
              s.xNext;
              s.ch match {
                case '/' => exit = true;            // end tag
                case '!' => val _ = s.xComment;
                case _   => ts.append( xExpr ); // parse child
              }

            case '{' =>
              if( s.xCheckScalaBlock ) {
                xScalaExpr( ts );
              } else {
                val str = new StringBuffer("{");
                str.append( s.xText );
                ts.append( makeText( s.pos, str.toString() ));
              }

            case '&' => // EntityRef or CharRef
              s.xNext;
              s.ch match {
                case '#' => // CharacterRef
                  s.xNext;
                  val theChar = makeText( s.pos, s.xCharRef );
                  s.xToken(';');
                  ts.append( theChar);
                case _ => // EntityRef
                  val pos = s.pos;
                  val n = s.xName ;
                  s.xToken(';');
                  ts.append( makeEntityRef( s.pos, n ));
              }
            case _ => // text content
              ts.append( makeText( s.pos, s.xText ));
          }
        }
      }
      xEndTag( elemName );
      makeXML( pos, elemName, ts.toArray(), attrMap );
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
    val pos = s.pos;
    var tree = xPattern; s.token = EMPTY; s.nextToken();
    if( s.xStartsXML )  {
      val ts = new myTreeList(); ts.append( tree );
      while( s.xStartsXML ) { ts.append( xPattern ); s.nextToken(); }
      tree = makeXMLseqPat( pos, ts.toArray() );
    }
    tree
  }



  /** '<' xPattern  ::= Name [S] { xmlPattern | '{' pattern3 '}' } ETag
   *                  | Name [S] '/' '>'
   */
  def xPattern:Tree = {
    //Console.println("xPattern");
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
            ts.append( makeText( pos, s.xText ) );
	}
      }
      xEndTag( elemName );
      makeXMLpat( pos, elemName, ts.toArray() );
    }
  }

} /* class MarkupParser */
}
