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
import scalac.util.{ Name, Names, TypeNames } ;
package scala.tools.scalac.ast.parser {

/** this class builds instance of Tree that represent XML */
class SymbolicXMLBuilder(make: TreeFactory, gen: TreeGen, p: Parser, preserveWS: Boolean ) {

  import scala.tools.scalac.ast.{TreeList => myTreeList}

  val  _ArrayBuffer = Name.fromString("ArrayBuffer");
  val  _Attribute = Name.fromString("Attribute");
  val  _NodeBuffer = Name.fromString("NodeBuffer");
  val  _NoAttributes = Name.fromString("NoAttributes");
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

  private def _scala_xml_Node_NoAttributes( pos: int ) =
    make.Select( pos, _scala_xml( pos, _Node ), _NoAttributes );

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

  private def _scala_xml_Attribute( pos: int ) =
    _scala_xml( pos, _Attribute );



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

  /**
   *  @arg  namespace: a Tree of type defs.STRING_TYPE
   *  @arg  label:     a Tree of type defs.STRING_TYPE
   *  @todo map:       a map of attributes !!!
   */

  protected def mkXML(pos: int, isPattern: boolean, namespace: Tree, label: Tree, attrs: Array[Tree], children: Array[Tree]): Tree = {
    if( isPattern ) {
      val ts = new myTreeList();
      ts.append( namespace );
      ts.append( label );
      ts.append( new Tree$Ident( Names.PATTERN_WILDCARD ) ); // attributes?
      ts.append( convertToTextPat( children ) );
      make.Apply(pos,
                 convertToTypeId( _scala_xml_Elem( pos ) ),
                 ts.toArray())
    } else {
      val ab = new scala.collection.mutable.ArrayBuffer[Tree]();
      ab + namespace;
      ab + label;
      if(( attrs.length ) == 0 )
        ab + _scala_xml_Node_NoAttributes( pos )
      else
        ab + make.Apply(pos,
                        make.Select( pos,
                                    _scala_xml_Node_NoAttributes( pos ),
                                    Names.PERCENT ),
                        attrs);
      if(( children.length ) > 0 )
        ab + make.Typed(pos,
                        makeXMLseq(pos, children ),
                        make.Ident(pos, TypeNames.WILDCARD_STAR));
      val arr:Array[Tree] = new Array[Tree]( ab.length );
      ab.elements.copyToArray( arr, 0 );
      make.Apply( pos, _scala_xml_Elem( pos ), arr )
    }
  }

  final def EntityRef( pos:int, n:Name ) = {
    val constr = make.Apply( pos,
                            _scala_xml_EntityRef( pos ),
                            Predef.Array[Tree]( gen.mkStringLit( pos, n.toString() )));

    make.New( pos, constr );
  };
  // create scala.xml.Text here <: scala.xml.Node
  def makeText( pos: int, isPattern:Boolean, txt:String ):Tree =  {
    //makeText( pos, isPattern, gen.mkStringLit( pos, txt ));
    val txt1 = gen.mkStringLit( pos, txt );
    if( isPattern )
      makeTextPat( pos, txt1 );
    else
      makeText1( pos, txt1 );
  }

  def appendTrimmed(pos: int, mode:Boolean, ts:myTreeList, txt:String) = {
    var textNodes =
      if( !preserveWS )
        new TextBuffer().append( txt ).toText;
      else
        List( Text( txt ));

    for( val t <- textNodes )
      ts.append( makeText( pos, mode, t.text ));
  }

  // create scala.xml.Text here <: scala.xml.Node
/*
  protected def makeText( pos: int, isPattern:Boolean, txt:Tree ):Tree = {
    if( isPattern )
      makeTextPat( pos, txt );
    else
      makeText1( pos, txt );
  }
*/
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
  def makeAttribute( pos: int, ns:String, key:String, value:Tree ):Tree =
    make.Apply(pos,
               _scala_xml_Attribute(pos),
               Predef.Array[Tree] (
                 make.Ident( pos, Name.fromString( ns )),
                 gen.mkStringLit( pos, key ),
                 value
               )
             );

  // create
  def Comment( pos: int, comment:scala.xml.Comment ):Tree =
    Comment( pos, gen.mkStringLit( pos, comment.text ));

  // create
  def CharData( pos: int, charData:scala.xml.CharData ):Tree =
    CharData( pos, gen.mkStringLit( pos, charData.text ));

  // create scala.xml.Text here <: scala.xml.Node
  def ProcInstr( pos: int, procInstr:scala.xml.ProcInstr ):Tree =
    procInstr.text match {
      case Some(txt) =>
        ProcInstr( pos,
                   gen.mkStringLit( pos, procInstr.target ),
                   makeSome( pos, gen.mkStringLit( pos, txt )));
      case _ =>
        ProcInstr( pos,
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

  def makeNodeBuffer(pos: int): Tree = {
    val constr = make.Apply( pos,
                            _scala_xml_NodeBuffer( pos ),
                            Tree.EMPTY_ARRAY);
    make.New( pos, constr );
  }

  protected def CharData(pos: int, txt: Tree):Tree = {
    val constr = make.Apply( pos,
                           _scala_xml_CharData( pos ),
                           Predef.Array[Tree] ( txt ));
    make.New( pos, constr );
  }

  protected def Comment(pos: int, txt: Tree):Tree = {
    val constr = make.Apply( pos,
                           _scala_xml_Comment( pos ),
                            Predef.Array[Tree] ( txt ));
    make.New( pos, constr );
  }

  protected def ProcInstr(pos: int, target: Tree, txt: Tree): Tree = {
    val constr = make.Apply( pos,
                           _scala_xml_ProcInstr( pos ),
                            Predef.Array[Tree] ( target, txt ));
    make.New( pos, constr );
  }

  /** @todo: attributes */
  def makeXMLpat(pos: int, n: Name, args: Array[Tree]): Tree =
    mkXML(pos,
          true,
          new Tree$Ident( Names.PATTERN_WILDCARD ):Tree,
          gen.mkStringLit( pos, n.toString() ):Tree,
          Tree.EMPTY_ARRAY:Array[Tree],
          args:Array[Tree]);

  protected def convertToTextPat(t: Tree): Tree = t match {
    case _:Tree$Literal => makeTextPat(t.pos, t);
    case _ => t
  }

  protected def convertToTextPat(ts: Array[Tree]): Array[Tree] = {
    var res:Array[Tree] = null;
    var i = 0; while( i < ts.length ) {
        val t1 = ts( i );
        val t2 = convertToTextPat( t1 );
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
                     Predef.Array[Tree]( args( i ) ))
        )
      }
      i = i + 1;
    }
    make.Block( pos, ts.toArray(), nIdent );
  }

  def makeXMLseqPat( pos:int, args:Array[Tree] ) = {
    make.Apply( pos, _scala_Seq( pos ), args );
  }



  def getPrefix( name:String ):Option[String] = {
    val i = name.indexOf(':');
    if( i != -1 ) Some( name.substring(0, i) ) else None
  }

  def qualified( pos:Int, name:String ):Pair[String,String] =
    getPrefix( name ).match {
      case Some( pref ) =>
        val newLabel = name.substring( pref.length()+1, name.length() );
        // if( newLabel.indexOf(':') != -1 )  syntaxError
        Pair( "namespace$"+pref, newLabel );
      case None =>
        Pair( "namespace$default", name );
    }

  /** makes an element */
  def makeXML(pos: int, labeln: Name, attrMap1: ListMap[String,Tree], args: Array[Tree]): Tree={
    var label = labeln.toString();
    var setNS = ListMap.Empty[String, Tree];
    var attrMap = attrMap1;

    for( val z <- attrMap.keys; z.startsWith("xmlns") ) {
      val i = z.indexOf(':');
      if( i == -1 )
        setNS = setNS.update("default", attrMap( z ));
      else {
        val zz = z.substring( i+1, z.length() );
        setNS = setNS.update( zz, attrMap( z ));
      }
      attrMap = attrMap - z;
    }
    val i = label.indexOf(':');
    val Pair( namespace, newlabel ) = qualified( pos, label );

    var attr:Array[Tree] =
      if( attrMap.isEmpty )
        Tree.EMPTY_ARRAY
      else {
        val attrs:Array[Tree] = new Array[Tree](attrMap.size);
        var k = 0;
        var it = attrMap.elements;
        while( it.hasNext ) {
          val ansk = it.next;
          val Pair( ns, aname ) = qualified( pos, ansk._1 );
          attrs( k ) = makeAttribute( pos, ns, aname, ansk._2 );
          k = k + 1;
        }
        attrs
      }

    var t = mkXML(pos,
                  false,
                  make.Ident(pos, Name.fromString(namespace)):Tree,
                  gen.mkStringLit(pos, newlabel):Tree,
                  attr:Array[Tree],
                  args:Array[Tree]);
    if( !setNS.isEmpty ) {
      val nsStms = new Array[Tree]( setNS.size );
      var i = 0;
      for( val Pair(ns:String, uri:Tree) <- setNS.toList ) {
        nsStms( i ) = setNamespacePrefix(pos, ns, uri );
        i = i + 1;
      }
      make.Block( pos, nsStms, t )
    } else
      t
  }

  def setNamespacePrefix(pos:Int, pref:String, uri:Tree) =
    make.ValDef(pos, 0, Name.fromString("namespace$"+pref), Tree.Empty, uri);


}
}
