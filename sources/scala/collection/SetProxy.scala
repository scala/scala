/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection;


/** This is a simple wrapper class for <code>scala.collection.Set</code>.
 *  It is most useful for assembling customized set abstractions
 *  dynamically using object composition and forwarding.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 21/07/2003
 */
class SetProxy[A](set: Set[A]) extends Set[A] with IterableProxy(set) {

    def size: Int = set.size;

    override def isEmpty: Boolean = set.isEmpty;

    def contains(elem: A): Boolean = set.contains(elem);

    override def subsetOf(that: Set[A]): Boolean = set.subsetOf(that);

    override def foreach(f: A => Unit): Unit = set.foreach(f);

    override def exists(p: A => Boolean): Boolean = set.exists(p);

    override def toList: List[A] = set.toList;
}
