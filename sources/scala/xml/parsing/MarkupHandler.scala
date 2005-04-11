package scala.xml.parsing;

import scala.collection.immutable ;
import scala.collection.mutable ;
import scala.collection.Map ;

/** class that handles markup - provides callback methods to MarkupParser */
abstract class MarkupHandler {

  /** mapping from prefixes to namespaces
  var namespace: immutable.Map[String,String] =
    new immutable.TreeMap[String,String]
    .update("","")
    .update("xml","http://www.w3.org/XML/1998/namespace");
    */

  /** callback method that is invoked by MarkupParser after fully
   *    parsing element fully.
   *
   *  @param pos      the position in the sourcefile
   *  @param pre      the prefix
   *  @param label    the local name
   *  @param attrs    the attributes (metadata)
   *  @param args     the children of this element
   */
  def element(pos: int, pre: String, label: String, attrs: MetaData, scope:NamespaceBinding, args: NodeSeq): NodeSeq;

  def procInstr(pos: Int, target: String, txt: String): NodeSeq;

  def comment(pos: Int, comment: String ): NodeSeq;

  def entityRef(pos: Int, n: String): NodeSeq;

  def text(pos: Int, txt:String): NodeSeq;

}
