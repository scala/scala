/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.mutable;


/** This is a simple wrapper class for <code>scala.collection.mutable.Map</code>.
 *  It is most useful for assembling customized map abstractions
 *  dynamically using object composition and forwarding.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 21/07/2003
 */
class MapWrapper[A, B](m: Map[A, B]) extends Map[A, B]
			                         with scala.collection.MapWrapper[A, B](m) {

    def update(key: A, value: B): Unit = m.update(key, value);

    def -=(key: A): Unit = m.-=(key);

    override def incl(mappings: Pair[A, B]*): Unit = m.incl(mappings);

    override def incl(map: Iterable[Pair[A, B]]): Unit = m.incl(map);

    override def excl(keys: A*): Unit = m.excl(keys);

    override def excl(keys: Iterable[A]): Unit = m.excl(keys);

    override def clear: Unit = m.clear;

    override def map(f: (A, B) => B): Unit = m.map(f);

    override def filter(p: (A, B) => Boolean): Unit = m.filter(p);

    override def toString() = m.toString();

    override def mappingToString(p: Pair[A, B]) = m.mappingToString(p);
}
