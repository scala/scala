/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;

import scala.xml.{Node,Text};
import scala.xml.nobinding.Element;
import scala.collection.immutable.ListMap ;

/** Instances of <code>Symbol</code> can be created easily with
 *  Scala's built-in quote mechanism. For instance, the Scala term
 *  <code>'mysym</code> will invoke the constructor of the
 *  <code>Symbol</code> class in the following way:
 *  <code>new Symbol("mysym")</code>.
 *
 *  @author  Martin Odersky
 *  @version 1.0, 08/08/2003
 */
case class Symbol(name: String) {

  var map : ListMap[String,String] = ListMap.Empty;

  override def toString() = "'" + name;

  def % (ch:Node*) = new Element(this, List.fromIterator(ch.elements)) {
    override def attributes = map;
  };

  def % (a:Attribute) =  {
    map = map.update(a.name, a.value);
    this
  }
  def -> (value:String) = new Attribute( name, value );

  class Attribute( n:String, v:String ) {
    final val name = n;
    final val value = v;
  };

}

