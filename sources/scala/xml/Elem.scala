/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.xml;

import scala.collection.immutable;

/** The case class <code>Elem</code> implements the Node trait,
 *  providing an immutable data object representing an XML element.
 *
 *  @param namespace the namespace code as assigned by NamespaceRegistry
 *  @param label the element name
 *  @param attribute the attribute map
 *  @param child the children of this node
 *  @author  Burak Emir
 */
case class Elem( namespace$$:String, label$$: String, attribute:immutable.Map[String,String], child: Node*) extends Node {

  final val namespaceIntern     = namespace$$.intern();
  final def namespace  = namespaceIntern;

  final val labelIntern = label$$.intern();
  final def label       = labelIntern;

  final override def typeTag$:Int = 0;

  def this(namespace: String, label: String, child: Node*) =
    this(namespace, label, Node.NoAttributes, child:_*);

  def this(label: String, child: Node*) =
    this(Node.EmptyNamespace, label, Node.NoAttributes, child:_*);

  /** Return a new element with updated attributes
   *
   *  @param attrs
   *  @return a new symbol with updated attributes
   */
  final def %(attrs: Seq[Pair[String, String]]): Elem = {
    var newmap = new immutable.TreeMap[String, String]();
    for ( val p <- attribute.elements ) {
      newmap = newmap.update( p._1, p._2 )
    }
    for ( val p <- attrs ) {
      newmap = newmap.update( p._1, p._2 )
    }
    Elem(namespace, label, newmap, child:_*)
  }

  /** Return a new symbol with updated attribute
   *
   *  @param attr
   *  @return a new symbol with updated attribute
   */
  final def %(attr: Pair[String, String]): Elem = {
    var newmap = new immutable.TreeMap[String, String]();
    for ( val p <- attribute.elements ) {
      newmap = newmap.update( p._1, p._2 )
    }
    newmap = newmap.update( attr._1, attr._2 );
    Elem(namespace,  label, newmap, child:_*)
  }

}
