package scala.xml.parsing;

import scala.collection.immutable ;
import scala.collection.mutable ;
import scala.collection.Map ;

/** class that handles markup - provides callback methods to MarkupParser */
abstract class MarkupHandler[A] {

  /** a stack of prefix namespace mappings */
  protected val prefixStack =
    new mutable.Stack[immutable.Map[String,String]]();

  /** mapping from prefixes to namespaces */
  var namespace: immutable.Map[String,String] =
    new immutable.TreeMap[String,String]
    .update("","")
    .update("xml","http://www.w3.org/XML/1998/namespace");


  var tmpPrefix: mutable.Map[String, String] =
    new mutable.HashMap[String,String];

  /** returns prefix of the qualified name if any */
  final def namespacePrefix(name: String): Option[String] = {
    val i = name.indexOf(':');
    if( i != -1 ) Some( name.substring(0, i) ) else None
  }

  /** removes xmlns attributes from attr as a side effect, and returns a prefix
   *  map resulting from them
   * /
  final def namespaceDecl1(aMap: mutable.Map[String, AttribValue]): Map[String, String] = {
    val setNS = new mutable.HashMap[String, String];
    / * DEBUG * /
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
  */

  /** removes xmlns attributes from attr as a side effect, and returns a prefix
   *  map resulting from them
   */
  final def internal_namespaceDecl(prefix:String, uri:String): Unit = {
    tmpPrefix.update(prefix, uri);
  }

  def attributeCDataValue(pos: int, str:String): AttribValue;
  def attributeNamespaceDecl(pos: int, uri: String): AttribValue;

  def attribute(pos: int, uri: String, key: String, value:String): AttribValue =
    attributeCDataValue(pos, value);

  /** be careful to copy everything from attrMap1, as it will change
   *  @param pos      the position in the sourcefile
   *  @param uri      the namespace uri
   *  @param label    the tag name
   *  @param attrMap1 the attribute map, from Pair(uri,label) to target
   *  @param args     the children of this element
   */
  def element(pos: int, uri: String, label: String, attrMap1: mutable.Map[Pair[String,String],AttribValue], args: mutable.Buffer[A]): Iterable[A];

  def charData(pos: Int, txt: String ): Iterable[A];
  def procInstr(pos: Int, target: String, txt: String): Iterable[A];
  def comment(pos: Int, comment: String ): Iterable[A];
  def entityRef(pos: Int, n: String): Iterable[A];

  def text(pos: Int, txt:String): Iterable[A];


  def internal_startPrefixMapping: Unit = {
    this.prefixStack.push( this.namespace );
    this.namespace = this.namespace incl tmpPrefix;
    tmpPrefix.clear;
  }

  def internal_endPrefixMapping: Unit = {
    this.namespace = prefixStack.pop;
  }

}
