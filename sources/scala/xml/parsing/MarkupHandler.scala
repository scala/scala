package scala.xml.parsing;

import scala.collection.immutable ;
import scala.collection.mutable ;
import scala.collection.Map ;

/** @todo: make ConstructingMarkupHandler */
abstract class MarkupHandler[MarkupType, AVType] {

  /** a stack of prefix namespace mappings */
  protected val prefixStack =
    new mutable.Stack[immutable.Map[String,String]]();

  /** mapping from prefixes to namespaces */
  var namespace: immutable.Map[String,String] =
    new immutable.TreeMap[String,String];

  /** returns prefix of the qualified name if any */
  final def namespacePrefix(name: String): Option[String] = {
    val i = name.indexOf(':');
    if( i != -1 ) Some( name.substring(0, i) ) else None
  }

  /** removes xmlns attributes from attr as a side effect, and returns a prefix
   *  map resulting from them
   */
  final def namespaceDecl(aMap: mutable.Map[String, AttribValue]): Map[String, String] = {
    val setNS = new mutable.HashMap[String, String];
    /* DEBUG */
    val attrIt = aMap.keys;
    while( attrIt.hasNext ) {
      val z = attrIt.next;
      if( z.startsWith("xmlns") ) {
        val uri = aMap( z ) match {
          case NamespaceDecl(uri1) => uri1;
          case _                   => throw FatalError("bad namespace declaration");
        }
        val i = z.indexOf(':');
        if( i == -1 )
          setNS.update("", uri );
        else {
          val zz = z.substring( i+1, z.length() );
          setNS.update( zz, uri );
        }
        aMap -= z;
      }
    }
    setNS;
  }

  def attributeCDataValue(pos: int, str:String): AttribValue;
  def attributeNamespaceDecl(pos: int, uri: String): AttribValue;

  /** be careful to copy everything from attrMap1, as it will change
   *  @param attrMap1 the attribute map.
   */
  def element(pos: int, uri: String, label: String, attrMap1: mutable.Map[String,AttribValue], args: mutable.Buffer[MarkupType]): MarkupType;

  def charData(pos: Int, txt: String ): MarkupType;
  def procInstr(pos: Int, target: String, txt: String): MarkupType;
  def comment(pos: Int, comment: String ): MarkupType;
  def entityRef(pos: Int, n: String): MarkupType;

  def text(pos: Int, txt:String): MarkupType;


  def internal_startPrefixMapping(pref: Map[String, String]) = {
    if( !pref.isEmpty ) {
      this.prefixStack.push( this.namespace );
      this.namespace incl pref;
    }
  }

  def internal_endPrefixMapping(pref: Map[String, String]): Unit = {
    if( !pref.isEmpty ) {
      this.namespace = prefixStack.pop;
    }
  }

}
