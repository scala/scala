/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection;


/** This is a simple wrapper class for <code>scala.collection.Map</code>.
 *  It is most useful for assembling customized map abstractions
 *  dynamically using object composition and forwarding.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 21/07/2003
 */
class MapProxy[A, +B](map: Map[A, B]) extends Map[A, B] {

    def size: Int = map.size;

    def get(key: A): Option[B] = map.get(key);

    override def isEmpty: Boolean = map.isEmpty;

    override def apply(key: A): B = map.apply(key);

    override def contains(key: A): Boolean = map.contains(key);

    override def isDefinedAt(key: A) = map.isDefinedAt(key);

    override def keys: Iterator[A] = map.keys;

    override def values: Iterator[B] = map.values;

    override def foreach(f: (A, B) => Unit) = map.foreach(f);

    override def toList: List[Pair[A, B]] = map.toList;

    override def toString() = map.toString();

    def elements: Iterator[Pair[A, B]] = map.elements;
}
