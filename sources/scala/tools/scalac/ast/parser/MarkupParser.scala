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
import scalac.util.{ Name, Names, TypeNames };
import scala.tools.util.Position;
import java.lang.{Integer, Long, Float, Double};
import scala.Iterator;
import scala.tools.scalac.util.NewArray;
import scala.collection.immutable.ListMap ;
import scala.collection.mutable.Buffer;
import scala.xml.{Text,TextBuffer};

package scala.tools.scalac.ast.parser {

class MarkupParser( unit:Unit, s:Scanner, p:Parser, preserveWS:boolean ) {

  import Tokens.{EMPTY, LBRACE, RBRACE} ;
  import scala.tools.scalac.ast.{TreeList => myTreeList}

  val  _ArrayBuffer = Name.fromString("ArrayBuffer");
  val  _NodeBuffer = Name.fromString("NodeBuffer");
  val  _TreeMap = Name.fromString("TreeMap");
  val  _Elem = Name.fromString("Elem");
  val  _Seq = Name.fromString("Seq");
  val  _String = Name.fromString("String");
  val  _immutable = Name.fromString("immutable");
  val  _mutable = Name.fromString("mutable");
  val  _append = Name.fromString("append");
  val  _plus = Name.fromString("$plus");
  val  _collection = Name.fromString("collection");
  val  _xml = Name.fromString("xml");
  val  _Comment = Name.fromString("Comment");
  val  _CharData = Name.fromString("CharData");
  val  _Node = Name.fromString("Node");
  val  _None = Name.fromString("None");
  val  _Some = Name.fromString("Some");
  val  _ProcInstr = Name.fromString("ProcInstr");
  val  _Text = Name.fromString("Text");
  val  _EntityRef = Name.fromString("EntityRef");

  /** the tree factory
   */
  val make: TreeFactory = unit.global.make;

  /** the tree generator
  */
  val gen: TreeGen = unit.global.treeGen;

  var mode:boolean = false;
  final val PATTERN = true;
  final val EXPR    = false;

  // convenience methods
  private def _scala( pos: int, name: Name ) =
    make.Select( pos, make.Ident( pos, Names.scala ), name );

  private def _scala_Seq( pos: int ) =
    p.convertToTypeId( _scala( pos, _Seq ));


  private def _scala_None( pos: int ) =
    _scala( pos, _None ) ;

  private def _scala_Some( pos: int ) =
    p.convertToConstr( _scala( pos, _Some ));


  private def _string( pos: int ) =
    p.convertToTypeId( make.Ident( pos, _String ) );

  private def _scala_xml( pos: int, name: Name ) =
    make.Select( pos, _scala( pos, _xml ), name );

  private def _scala_xml_Node( pos: int ) =
    _scala_xml( pos, _Node );

  private def _scala_xml_NodeBuffer( pos: int ) =
    p.convertToConstr( _scala_xml( pos, _NodeBuffer ));

  private def _scala_xml_EntityRef( pos: int ) =
    p.convertToConstr( _scala_xml( pos, _EntityRef ));

  private def _scala_xml_Comment( pos: int ) =
    p.convertToConstr( _scala_xml( pos, _Comment ));

  private def _scala_xml_CharData( pos: int ) =
    p.convertToConstr( _scala_xml( pos, _CharData ));

  private def _scala_xml_ProcInstr( pos: int ) =
    p.convertToConstr( _scala_xml( pos, _ProcInstr ));

  private def _scala_xml_Text( pos: int ) =
    _scala_xml( pos, _Text );


  private def _scala_collection( pos: int, name: Name ) =
    make.Select( pos, _scala( pos, _collection ), name );

  private def _scala_collection_mutable( pos: int, name: Name ) =
    make.Select(pos, _scala_collection(pos, _mutable ), name);

  private def _scala_collection_immutable( pos: int, name: Name ) =
    make.Select(pos, _scala_collection(pos, _immutable ), name);

  private def _scala_collection_mutable_ArrayBuffer( pos: int ) =
    make.Apply( pos,
               make.AppliedType(pos,
                                p.convertToConstr(
                                  _scala_collection_mutable(pos, _ArrayBuffer )),
                                Predef.Array[Tree](
                                  convertToTypeId(
                                    _scala_xml_Node( pos ) ))
                              ),
               Tree.EMPTY_ARRAY );

  private def _scala_collection_immutable_TreeMap( pos: int ) =
    make.Apply( pos,
               make.AppliedType(pos,
                                p.convertToConstr(
                                  _scala_collection_immutable(pos, _TreeMap )),
                                Predef.Array[Tree](
                                  _string( pos ),
                                  _string( pos )
                                )
                              ),
               Tree.EMPTY_ARRAY );

  private def _emptyMap( pos:int ) = {
    make.New( pos,_scala_collection_immutable_TreeMap( pos: int ));
  }

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
      ts.append( new Tree$Ident( Names.PATTERN_WILDCARD ) );
      ts.append( t );
      ts.append( new Tree$Ident( Names.PATTERN_WILDCARD ) );
      ts.append( convertToText( true, args ) );
      make.Apply(pos,
                 convertToTypeId( _scala_xml_Elem( pos ) ),
                 ts.toArray())
    } else {
      val constrArgs = if( 0 == args.length ) {
        Predef.Array[Tree]( gen.mkIntLit(pos, 0), t, _emptyMap( pos ) )
      } else {
        Predef.Array[Tree]( gen.mkIntLit(pos, 0), t, _emptyMap( pos ), make.Typed(
          pos, makeXMLseq(pos, args ), make.Ident(pos, TypeNames.WILDCARD_STAR)))
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
  def makeText( pos: int, isPattern:Boolean, txt:String ):Tree =  {
    makeText( pos, isPattern, gen.mkStringLit( pos, txt ));
  }

  def appendTrimmed(pos: int, ts:myTreeList, txt:String) = {
    var textNodes =
      if( !preserveWS )
        new TextBuffer().append( txt ).toText;
      else
        List( Text( txt ));

    for( val t <- textNodes )
      ts.append( makeText( s.pos, mode, t.text ));
  }

  // create scala.xml.Text here <: scala.xml.Node
  def makeText( pos: int, isPattern:Boolean, txt:Tree ):Tree = {
    if( isPattern )
      makeTextPat( pos, txt );
    else
      makeText1( pos, txt );
  }

  // create scala.xml.Text here <: scala.xml.Node
  def makeTextPat( pos: int, txt:Tree ):Tree = {
    return make.Apply(pos,
                      p.convertToConstr( _scala_xml_Text( pos ) ),
                      Predef.Array[Tree]( txt ));
  }

  def makeText1( pos: int, txt:Tree ):Tree = {
    val constr = make.Apply(pos,
                            p.convertToConstr( _scala_xml_Text( pos )),
                            Predef.Array[Tree]( txt ));
    make.New( pos, constr );
  }


  // create
  def makeComment( pos: int, comment:scala.xml.Comment ):Tree =
    makeComment( pos, gen.mkStringLit( pos, comment.text ));

  // create
  def makeCharData( pos: int, charData:scala.xml.CharData ):Tree =
    makeCharData( pos, gen.mkStringLit( pos, charData.text ));

  // create scala.xml.Text here <: scala.xml.Node
  def makeProcInstr( pos: int, procInstr:scala.xml.ProcInstr ):Tree =
    procInstr.text match {
      case Some(txt) =>
        makeProcInstr( pos,
                      gen.mkStringLit( pos, procInstr.target ),
                      makeSome( pos, gen.mkStringLit( pos, txt )));
      case _ =>
        makeProcInstr( pos,
                      gen.mkStringLit( pos, procInstr.target ),
                      makeNone( pos ));
    }

  def makeNone( pos: int ):Tree = _scala_None( pos );

  def makeSome( pos: int, txt:Tree ):Tree = {
    val constr = make.Apply( pos,
                           _scala_Some( pos ),
                           Predef.Array[Tree] ( txt ));
    make.New( pos, constr );
  }

  def makeNodeBuffer( pos: int ):Tree = {
    val constr = make.Apply( pos,
                            _scala_xml_NodeBuffer( pos ),
                            Tree.EMPTY_ARRAY);
    make.New( pos, constr );
  }

  def makeCharData( pos: int, txt:Tree ):Tree = {
    val constr = make.Apply( pos,
                           _scala_xml_CharData( pos ),
                           Predef.Array[Tree] ( txt ));
    make.New( pos, constr );
  }

  def makeComment( pos: int, txt:Tree ):Tree = {
    val constr = make.Apply( pos,
                           _scala_xml_Comment( pos ),
                            Predef.Array[Tree] ( txt ));
    make.New( pos, constr );
  }

  def makeProcInstr( pos: int, target:Tree, txt:Tree ):Tree = {
    val constr = make.Apply( pos,
                           _scala_xml_ProcInstr( pos ),
                            Predef.Array[Tree] ( target, txt ));
    make.New( pos, constr );
  }


  def makeXMLpat(pos:int, n:Name, args:Array[Tree]):Tree =
    mkXML(pos, true, gen.mkStringLit( pos, n.toString() ), args);

  def makeXML(pos:int, n:Name, args:Array[Tree]):Tree =
    mkXML(pos, false, gen.mkStringLit( pos, n.toString() ), args);

  def convertToText(isPattern:Boolean, t:Tree):Tree = t match {
    case _:Tree$Literal => makeText(t.pos, isPattern, t);
    case _ => t
  }

  def convertToText( isPattern:Boolean, ts:Array[Tree] ):Array[Tree] = {
    var res:Array[Tree] = null;
    var i = 0; while( i < ts.length ) {
        val t1 = ts( i );
        val t2 = convertToText( isPattern, t1 );
        if (!t1.eq(t2)) {
          if( null == res ) {  // lazy copy
            res = new Array[Tree]( ts.length );
            System.arraycopy( ts, 0, res, 0, i );
          }
          res( i ) = t2;
        }
        i = i + 1;
    }
    if( null == res ) ts else res;
  }

  def isEmptyText(t:Tree) = t match {
    case Tree$Literal(atree.AConstant.STRING("")) => true;
    case _                   => false;
  }
  def makeXMLseq( pos:int, args:Array[Tree] ) = {
    val ts = new TreeList();
    //val blocArr = new Array[Tree] ( 1 + args.length );
    //val constr = _scala_collection_mutable_ArrayBuffer( pos );
    val _buffer = makeNodeBuffer( pos );
    val n = p.fresh();
    val nIdent = make.Ident(pos, n);
    //blocArr( 0 )
    ts.append( make.ValDef(pos,
                           0, n, Tree.Empty,
                           _buffer));

    var i = 0; while( i < args.length ) {
      val ipos = args(i).pos;
      if( !isEmptyText( args( i ))) {
        ts.append(
          make.Apply( ipos,
                     make.Select( ipos, nIdent, _plus /*_append*/ ),
                     Predef.Array[Tree]( convertToText(false, args( i ) )))
        )
      }
      i = i + 1;
    }
    make.Block( pos, ts.toArray(), nIdent );
  }

  def makeXMLseqPat( pos:int, args:Array[Tree] ) = {
    make.Apply( pos, _scala_Seq( pos ), args );
  }

  /** @todo: create map here directly */
  def makeXML(pos:int,n:Name,args:Array[Tree],attrMap:ListMap[Name,Tree]):Tree={
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
    mode = EXPR;
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
    if(n != s.xName) s.xSyntaxError( "expected closing tag of " + n );
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
    val Tuple2(elemName, attrMap) = xTag;
    if(s.ch == '/') { // empty element
      s.xToken('/');
      s.xToken('>');
      makeXML( pos, elemName, Tree.EMPTY_ARRAY, attrMap );
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
                    ts.append( makeCharData( pos, s.xCharData ));
                  else                              // comment
                    ts.append( makeComment( pos, s.xComment ));
                case '?' =>                         // PI
                  s.xNext;
                  ts.append( makeProcInstr( pos, s.xProcInstr ));
                case _   => ts.append( xExpr );     // child
              }

            case '{' =>
              if( s.xCheckScalaBlock ) {
                ts.append( xScalaExpr );
              } else {
                val str = new StringBuffer("{");
                str.append( s.xText );
                appendTrimmed( pos, ts, str.toString() )
              }
            // postcond: s.xScalaBlock == false!
            case '&' => // EntityRef or CharRef
              s.xNext;
              s.ch match {
                case '#' => // CharacterRef
                  s.xNext;
                  val theChar = makeText( s.pos, false, s.xCharRef );
                  s.xToken(';');
                  ts.append( theChar );
                case _ => // EntityRef
                  val pos = s.pos;
                  val n = s.xName ;
                  s.xToken(';');
                  ts.append( makeEntityRef( s.pos, n ));
              }
            case _ => // text content
              appendTrimmed( s.pos, ts, s.xText );
            // here s.xScalaBlock might be true

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
    val oldMode = mode;
    mode = PATTERN;
    val pos = s.pos;
    var tree = xPattern; s.token = EMPTY; s.nextToken();
    if( s.xStartsXML )  {
      val ts = new myTreeList(); ts.append( tree );
      while( s.xStartsXML ) { ts.append( xPattern ); s.nextToken(); }
      tree = makeXMLseqPat( pos, ts.toArray() );
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
    val elemName = s.xName;
    s.xSpaceOpt;
    if( s.ch == '/' ) { // empty tag
      s.xNext;
      s.xToken('>');
      return makeXMLpat( pos, elemName, Tree.EMPTY_ARRAY );
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
            appendTrimmed( pos, ts, s.xText );
          // here  s.xScalaBlock might be true;
          //if( s.xScalaBlock ) throw new ApplicationError("after:"+text); // assert
	}
    }
    xEndTag( elemName );
    makeXMLpat( pos, elemName, ts.toArray() );
  }

} /* class MarkupParser */
}
