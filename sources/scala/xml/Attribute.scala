/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.xml ;

/** An XML attribute
 *
 *  @todo allow for attributes that are not strings
 *
 *  @param namespace$$ the namespace URI
 *  @param key$$       the local attribute name
 *  @author  Burak Emir
 */
case class Attribute(namespace:String, key:String, value:String) with Ordered[Attribute] {

  def compareTo [b >: Attribute <% Ordered[b]](that: b): int = that match {
    case z:Attribute =>
      val i = key.compareTo( z.key );
      if( i != 0 ) i else namespace.compareTo( z.namespace )
    case _ => -(that.compareTo(this));
  }

}
