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
 *  @author  Burak Emir
 *  @version 1.0, 26/04/2004
 */
case class Elem( label: String, attribute:immutable.Map[String,String], child: Node*) extends Node {

  final override def typeTag$:Int = 0;

  def this(label: String, child: Node*) = this(label,immutable.ListMap.Empty[String,String],child:_*);

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
    return Elem( label, newmap, child:_* )
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
    return Elem( label, newmap, child:_* )
  }

}
