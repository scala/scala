/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002-2005, LAMP/EPFL         **
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
import scala.collection.immutable.{ Map, ListMap };
import scala.collection.mutable;
import scala.xml.{Text,TextBuffer};
import scalac.util.{ Name, Names, TypeNames } ;

package scala.tools.scalac.ast.parser {

/** this class builds instance of Tree that represent XML */
class SymbolicXMLBuilder(make: TreeFactory, gen: TreeGen, p: Parser, preserveWS: Boolean ) {

  import scala.tools.scalac.ast.{TreeList => myTreeList}

  var isPattern:Boolean = _;

  val _ArrayBuffer         = Name.fromString("ArrayBuffer");
  val _Attribute           = Name.fromString("Attribute");
  val _MetaData            = Name.fromString("MetaData");
  val _NamespaceBinding    = Name.fromString("NamespaceBinding");
  val _NodeBuffer          = Name.fromString("NodeBuffer");
  val _NoAttributes        = Name.fromString("NoAttributes");

  val _Null                = Name.fromString("Null");

  val _PrefixedAttribute   = Name.fromString("PrefixedAttribute");
  val _UnprefixedAttribute = Name.fromString("UnprefixedAttribute");
  val _TreeMap             = Name.fromString("TreeMap");
  val _Elem                = Name.fromString("Elem");
  val _Seq                 = Name.fromString("Seq");
  val _String              = Name.fromString("String");
  val _immutable           = Name.fromString("immutable");
  val _mutable             = Name.fromString("mutable");
  val _append              = Name.fromString("append");
  val _plus                = Name.fromString("$amp$plus");
  val _collection          = Name.fromString("collection");
  val _toList              = Name.fromString("toList");
  val _xml                 = Name.fromString("xml");
  val _Comment             = Name.fromString("Comment");
  val _CharData            = Name.fromString("CharData");
  val _Node                = Name.fromString("Node");
  val _None                = Name.fromString("None");
  val _Some                = Name.fromString("Some");
  val _ProcInstr           = Name.fromString("ProcInstr");
  val _Text                = Name.fromString("Text");
  val _EntityRef           = Name.fromString("EntityRef");

  // convenience methods
  private def _scala( pos: int, name: Name ) =
    make.Select( pos, make.Ident( pos, Names.scala ), name );

  private def _scala_Seq( pos: int ) =
    p.convertToTypeId( _scala( pos, _Seq ));


  private def _scala_None( pos: int ) =
    _scala( pos, _None ) ;

  private def _scala_Some(pos: int) =
    p.convertToConstr(_scala(pos, _Some));


  private def _string(pos: int) =
    p.convertToTypeId(make.Ident(pos, _String));

  private def _scala_xml(pos: int, name: Name) =
    make.Select( pos, _scala(pos, _xml), name );

  private def _scala_xml_MetaData(pos: int) =
     p.convertToTypeId(_scala_xml( pos, _MetaData ));

  private def _scala_xml_NamespaceBinding( pos: int ) =
     p.convertToTypeId(_scala_xml( pos, _NamespaceBinding ));

  private def _scala_xml_Null( pos: int ) =
     _scala_xml( pos, _Null );

  private def _scala_xml_PrefixedAttribute( pos: int ) =
    p.convertToConstr( _scala_xml( pos, _PrefixedAttribute ));

  private def _scala_xml_UnprefixedAttribute( pos: int ) =
    p.convertToConstr( _scala_xml( pos, _UnprefixedAttribute ));

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

  private def _scala_xml_Text(pos: int) =
    _scala_xml( pos, _Text );


  private def _scala_collection(pos: int, name: Name) =
    make.Select( pos, _scala( pos, _collection ), name );

  private def _scala_collection_mutable(pos: int, name: Name) =
    make.Select(pos, _scala_collection(pos, _mutable ), name);

  private def _scala_collection_immutable(pos: int, name: Name) =
    make.Select(pos, _scala_collection(pos, _immutable), name);

  private def _scala_collection_mutable_ArrayBuffer(pos: int) =
    make.Apply( pos,
               make.AppliedType(pos,
                                p.convertToConstr(
                                  _scala_collection_mutable(pos, _ArrayBuffer )),
                                Predef.Array[Tree](
                                  convertToTypeId(
                                    _scala_xml_Node(pos) ))
                              ),
               Tree.EMPTY_ARRAY );

  private def _scala_collection_immutable_TreeMap(pos: int) =
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

  private def _emptyMap(pos: Int) = {
    make.New( pos,_scala_collection_immutable_TreeMap( pos: int ));
  }

  private def _scala_Tuple2(pos: Int) =
    _scala( pos, Names.Tuple2 );

  private def _scala_xml_Elem(pos: Int) =
    _scala_xml( pos, _Elem );

  private def _scala_xml_Attribute(pos: Int) =
    _scala_xml( pos, _Attribute );


  private def bufferToArray(buf: mutable.Buffer[Tree]): Array[Tree] = {
    val arr = new Array[Tree]( buf.length );
    var i = 0;
    for (val x <- buf.elements) { arr(i) = x; i = i + 1; }
    arr;
  }

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

  protected def mkXML(pos: int, isPattern: boolean, pre: Tree, label: Tree, attrs: /*Array[*/Tree/*]*/ , scope:Tree, children: mutable.Buffer[Tree]): Tree = {
    if( isPattern ) {
      val ts = new mutable.ArrayBuffer[Tree]();
      ts.append( pre );
      ts.append( label );
      ts.append( new Tree$Ident( Names.PATTERN_WILDCARD ) ); // attributes?
      ts.append( new Tree$Ident( Names.PATTERN_WILDCARD ) ); // scope?
      convertToTextPat( children );
      ts ++ children;
      make.Apply(pos,
                 convertToTypeId( _scala_xml_Elem( pos ) ),
                 bufferToArray( ts ))
    } else {
      val ab = new scala.collection.mutable.ArrayBuffer[Tree]();
      ab + pre;
      ab + label;
      ab + attrs;
      ab + scope;

      /*
      if(( attrs.length ) == 0 )
        ab + _scala_xml_Node_NoAttributes( pos )
      else
        ab + make.Apply(pos,
                        make.Select( pos,
                                    _scala_xml_Node_NoAttributes( pos ),
                                    Names.PERCENT ),
                        attrs);
                        */
      if(( children.length ) > 0 )
        ab + make.Typed(pos,
                        makeXMLseq( pos, children ),
                        make.Ident(pos, TypeNames.WILDCARD_STAR));
      val arr:Array[Tree] = new Array[Tree]( ab.length );
      ab.elements.copyToArray( arr, 0 );
      make.Apply( pos, _scala_xml_Elem( pos ), arr )
    }
  }

  final def entityRef( pos:int, n: String ) = {
    val constr = make.Apply( pos,
                            _scala_xml_EntityRef( pos ),
                            Predef.Array[Tree]( gen.mkStringLit( pos, n )));

    make.New( pos, constr );
  };
  // create scala.xml.Text here <: scala.xml.Node
  final def text( pos: int, txt:String ):Tree =  {
    //makeText( pos, isPattern, gen.mkStringLit( pos, txt ));
    val txt1 = gen.mkStringLit( pos, txt );
    if( isPattern )
      makeTextPat( pos, txt1 );
    else
      makeText1( pos, txt1 );
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

  // code to create attribute
  /*
   scala.xml.Attribute(uri, "key", [value])
   */

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
  def comment( pos: int, text: String ):Tree =
    Comment( pos, gen.mkStringLit( pos, text ));

  // create
  def charData( pos: int, txt: String ):Tree =
    CharData( pos, gen.mkStringLit( pos, txt ));

  // create scala.xml.Text here <: scala.xml.Node
  def procInstr( pos: int, target: String, txt: String ):Tree =
    ProcInstr(pos,
              gen.mkStringLit( pos, target ),
              gen.mkStringLit( pos, txt ));

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
  def makeXMLpat(pos: int, n: String, args: mutable.Buffer[Tree]): Tree =
    mkXML(pos,
          true,
          new Tree$Ident( Names.PATTERN_WILDCARD ):Tree,
          gen.mkStringLit( pos, n ):Tree,
          null, //Predef.Array[Tree](),
          null,
          args);

  protected def convertToTextPat(t: Tree): Tree = t match {
    case _:Tree$Literal => makeTextPat(t.pos, t);
    case _ => t
  }

  protected def convertToTextPat(buf: mutable.Buffer[Tree]): Unit = {
    var i = 0; while( i < buf.length ) {
      val t1 = buf( i );
      val t2 = convertToTextPat( t1 );
      if (!t1.eq(t2)) {
        buf.remove(i);
        buf.insert(i,t2);
      }
      i = i + 1;
    }
  }

  def isEmptyText(t:Tree) = t match {
    case Tree$Literal(atree.AConstant.STRING("")) => true;
    case _                   => false;
  }
  def makeXMLseq( pos:int, args:mutable.Buffer[Tree] ) = {
    val ts = new TreeList();
    //val blocArr = new Array[Tree] ( 1 + args.length );
    //val constr = _scala_collection_mutable_ArrayBuffer( pos );
    var _buffer = makeNodeBuffer( pos );
    val n = p.fresh();
    val nIdent = make.Ident(pos, n);
    //blocArr( 0 )
    ts.append( make.ValDef(pos,
                           0, n, Tree.Empty,
                           _buffer));

    val it = args.elements;
    while( it.hasNext ) {
      val t = it.next;
      val tpos = t.pos;
      if( !isEmptyText( t )) {
        _buffer = make.Apply( tpos,
                              make.Select( tpos, _buffer, _plus ),
                              Predef.Array[Tree]( t ));
      }
    }

    _buffer =  make.Select( pos, _buffer, _toList );

    _buffer;
  }

  def makeXMLseqPat( pos:int, args:Array[Tree] ) = {
    make.Apply( pos, _scala_Seq( pos ), args );
  }



  /** returns Some(prefix) if pre:name, None otherwise */
  def getPrefix( name:String ):Option[String] = {
    val i = name.indexOf(':');
    if( i != -1 ) Some( name.substring(0, i) ) else None
  }

  /** splits */
  protected def qualifiedAttr( pos:Int, namespace:String, name:String ):Pair[String,String] = {
    getPrefix( name ) match {
      case Some( pref ) =>
        val newLabel = name.substring( pref.length()+1, name.length() );
        // if( newLabel.indexOf(':') != -1 )  syntaxError
        Pair( "namespace$"+pref, newLabel );
      case None =>
        Pair( namespace, name );
    }
  }
  protected def qualified( pos:Int, name:String ):Pair[String,String] =
    getPrefix( name ) match {
      case Some( pref ) =>
        val newLabel = name.substring( pref.length()+1, name.length() );
        // if( newLabel.indexOf(':') != -1 )  syntaxError
        Pair( "namespace$"+pref, newLabel );
      case None =>
        Pair( "namespace$default", name );
    }

  final val _md = Name.fromString("$md");
  final val _scope = Name.fromString("$scope");
  final val _tmpscope = Name.fromString("$tmpscope");

  /** makes an element */
  def element(pos: int, qname: String, attrMap: mutable.Map[String,Tree], args: mutable.Buffer[Tree]): Tree = {
    //Console.println("SymbolicXMLBuilder::element("+pos+","+qname+","+attrMap+","+args+")");
    var setNS = new mutable.HashMap[String, Tree];

    val tlist = new myTreeList();
    val tlist2 = new myTreeList();

    def handleNamespaceBinding(pre:String, uri:Tree): Unit = {
      val t = make.Assign(pos,
                          make.Ident(pos, _tmpscope),
                          make.New(pos,
                                   make.Apply(
                                     pos,
                                     p.convertToConstr(_scala_xml( pos, _NamespaceBinding )),
                                     Predef.Array[Tree](
                                       {if(pre==null)
                                         gen.mkNullLit(pos)
                                        else
                                          gen.mkStringLit(pos, pre)} ,
                                       uri,
                                       make.Ident(pos, _tmpscope)))
                                 )
                        );
      tlist.append( t );
      //Console.println("SymbolicXMLBuilder::handleNamespaceBinding:");
      //Console.println(t.toString());

    }

    /* DEBUG */
    val attrIt = attrMap.keys;
    while( attrIt.hasNext ) {
      val z = attrIt.next;
      if( z.startsWith("xmlns") ) {  // handle namespace
        val i = z.indexOf(':');
        if( i == -1 )
          handleNamespaceBinding(null, attrMap( z ));
          //setNS.update("default", attrMap( z ) );
        else {
          val zz = z.substring( i+1, z.length() );
          //setNS.update( zz, attrMap( z ) );
          handleNamespaceBinding(zz, attrMap( z ));
        }
        attrMap -= z;
      }
    }

    val moreNamespaces = (0 < tlist.length());

    /* */
    val i = qname.indexOf(':');

    var newlabel = qname;
    val pre = getPrefix(qname) match {
      case Some(p) =>
        newlabel = qname.substring(p.length()+1, qname.length());
        p;
      case None =>
        null
    }
    //val Pair( pre, newlabel ) = qualified( pos, label );
    //Console.println("SymbolicXMLBuilder::(489) "+pre);

    // make attributes

    def handlePrefixedAttribute(pre:String, key:String, value:Tree): Unit = {
      val t =
        make.Assign(pos,
                    make.Ident(pos, _md),
                    make.New(pos,
                             make.Apply(pos,
                                        _scala_xml_PrefixedAttribute(pos),
                                        Predef.Array[Tree](
                                          gen.mkStringLit(pos,pre),
                                          gen.mkStringLit(pos,key),
                                          value,
                                          make.Ident(pos, _md)
                                        )
                                      )
                           )
                  );
      tlist2.append( t );
     // Console.println("SymbolicXMLBuilder::handlePrefixed :");
     // Console.println(t.toString());
    }

    def handleUnprefixedAttribute(key:String, value:Tree): Unit = {
      val t = make.Assign(pos,
                          make.Ident(pos, _md),
                          make.New(pos,
                                   make.Apply(pos,
                                              _scala_xml_UnprefixedAttribute(pos),
                                              Predef.Array[Tree](
                                                gen.mkStringLit(pos,key),
                                                value,
                                                make.Ident(pos, _md)
                                              )
                                            )
                                 )
                        );
      tlist2.append(t);
      //Console.println("SymbolicXMLBuilder::handlePrefixed :");
      //Console.println(t.toString());

    }

    //var attr:Array[Tree] =
    //  if( attrMap.isEmpty )
    //    Tree.EMPTY_ARRAY ; //gen.mkNullLit(pos); //
    //  else {
    //    val attrs:Array[Tree] = new Array[Tree](attrMap.size);
        /* DEBUG */
    //    var k = 0;
    var it = attrMap.elements;
    while (it.hasNext) {
      val ansk = it.next;

      getPrefix(ansk._1) match {
        case Some(pre) =>
          val key = ansk._1.substring(pre.length()+1, ansk._1.length());
          handlePrefixedAttribute(pre, key, ansk._2);
        case None      =>
          handleUnprefixedAttribute(ansk._1, ansk._2);
      }
      //val Pair( ns, aname ) = qualifiedAttr( pos, namespace, ansk._1 );
      //attrs( k ) = makeAttribute( pos, ns, aname, ansk._2 );
      //k = k + 1;
    }
    /* */
    //  attrs

     // }

    val moreAttributes = (0 < tlist2.length());

    val ts = new mutable.ArrayBuffer[Tree]();

    if (moreNamespaces) {
      ts +  make.ValDef(pos,
                        Modifiers.MUTABLE,
                        _tmpscope,
                        _scala_xml_NamespaceBinding(pos),
                        make.Ident(pos, _scope));
      ts ++ tlist;
    }

    //Console.println("printing ts");
    //for(val t <- ts)
     // Console.println(t.toString());

    val ts2 = new mutable.ArrayBuffer[Tree]();

    if(moreNamespaces) {
      ts2 +  make.ValDef(pos, 0, _scope, Tree.Empty, make.Ident(pos, _tmpscope));
    }
    if(moreAttributes) {
      ts2 +  make.ValDef(pos,
                         Modifiers.MUTABLE,
                         _md,
                         _scala_xml_MetaData(pos),
                         _scala_xml_Null(pos));
      ts2 ++ tlist2;
    }
    //Console.println("printing ts2");
    //for(val t <- ts2)
    //  Console.println(t.toString());

    val stms:Array[Tree] = new Array[Tree]( ts.length );
    ts.elements.copyToArray( stms, 0 );

    val stms2:Array[Tree] = new Array[Tree]( ts2.length );
    ts2.elements.copyToArray( stms2, 0 );

    val makeSymbolicPrefix = {
      if (null == pre)
        gen.mkNullLit(pos)
      else
        gen.mkStringLit(pos,pre)
    }

    val makeSymbolicAttrs = {
      if (moreAttributes)
        make.Ident(pos, _md) //attr:Array[Tree],
      else
        _scala_xml_Null(pos)
    }

    var t = mkXML(pos,
                  false,
                  makeSymbolicPrefix,
                  gen.mkStringLit(pos, newlabel):Tree,
                  makeSymbolicAttrs,
                  make.Ident(pos, _scope),
                  args);
    /*
    Console.println("printing _pre");
    Console.println({if(null == pre) "null" else pre.toString()});
    Console.println("printing newlabel");
    Console.println(newlabel.toString());
    Console.println("printing _scope");
    Console.println(_scope.toString());
    Console.println("printing t");
    Console.println(t.toString());
    */

    if (0 < stms2.length) {
      t = make.Block(pos, stms2, t);
    }
    ;
    if (0 < stms.length) {
      t = make.Block(pos, stms, t);
    }
    ;
    //Console.println("SymbolicXMLBuilder::element returns :");
    //Console.println(res.toString());

    t
    /* DEBUG
    if (! setNS.isEmpty) {
      val nsStms = new Array[Tree](setNS.size);
      var i = 0;
      for (val Pair(ns:String, uri:Tree) <- setNS.toList) {
        nsStms( i ) = setNamespacePrefix(pos, ns, uri );
        i = i + 1;
      }
      make.Block( pos, nsStms, t )
      } else {
      t
      } */

  }

  def setNamespacePrefix(pos:Int, pref:String, uri:Tree) =
    make.ValDef(pos, 0, Name.fromString("namespace$"+pref), Tree.Empty, uri);

}

} // package
