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
class MapProxy[A, B](m: Map[A, B]) extends Map[A, B]
                                   with scala.collection.MapProxy[A, B](m) {

    def update(key: A, value: B): Unit = m.update(key, value);

    override def ++=(map: Iterable[Pair[A, B]]): Unit = m ++= map;

    override def ++=(it: Iterator[Pair[A, B]]): Unit = m ++= it;

    override def incl(mappings: Pair[A, B]*): Unit = m ++= mappings;

    def -=(key: A): Unit = m -= key;

    override def --=(keys: Iterable[A]): Unit = m --= keys;

    override def --=(it: Iterator[A]): Unit = m --= it;

    override def excl(keys: A*): Unit = m --= keys;

    override def clear: Unit = m.clear;

    override def map(f: (A, B) => B): Unit = m.map(f);

    override def filter(p: (A, B) => Boolean): Unit = m.filter(p);

    override def toString() = m.toString();

    override def mappingToString(p: Pair[A, B]) = m.mappingToString(p);

    override def <<(cmd: Message[Pair[A, B]]): Unit = m << cmd;

    override def clone(): Map[A, B] = new MapProxy(m.clone());
}
