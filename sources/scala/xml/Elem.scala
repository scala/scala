/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */


package scala.xml;

import scala.collection.mutable.HashMap;


/** The case class <code>Elem</code> implements...
 *
 *  @author  Burak Emir
 *  @version 1.0, 26/04/2004
 */
case class Elem(label: String, child: Node*) extends Node {

  def similar(x: Any) = x match {
    case that: Node => (label == that.label) && child.equals(that.child)
    // sameElements
    case _ => false
  }

  def `@` = new HashMap[String, String]();

  /** the attributes axis - default is Nil
   *
   *  @return a list
   */
  def attribute: Seq[Pair[String, String]] = `@`.toList;

  /** Return a new element with updated attributes
   *
   *  @param attrs
   *  @return a new symbol with updated attributes
   */
  final def %(attrs: Seq[Pair[String, String]]): Elem = {
    val newmap = new HashMap[String, String]();
    for ( val p <- `@`.elements ) { newmap += p._1 -> p._2 };
    for ( val p <- attrs )        { newmap += p._1 -> p._2 };
    new Elem(label, child:_*) {
      override def `@` = newmap;
      override def attribute = `@`.toList;
    }
  }

  /** Return a new symbol with updated attribute
   *
   *  @param attr
   *  @return a new symbol with updated attribute
   */
  final def %(attr: Pair[String, String]): Elem = {
    val newmap = new HashMap[String, String]();
    for ( val p <- `@`.elements ) { newmap += p._1 -> p._2 };
    newmap += attr._1 -> attr._2;
    new Elem(label, child:_*) {
      override def `@` = newmap;
      override def attribute = `@`.toList;
    }
  }

}
