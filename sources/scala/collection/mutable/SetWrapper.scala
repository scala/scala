/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.mutable;


/** This is a simple wrapper class for <code>scala.collection.mutable.Set</code>.
 *  It is most useful for assembling customized set abstractions
 *  dynamically using object composition and forwarding.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 21/07/2003
 */
class SetWrapper[A](set: Set[A]) extends Set[A]
                                 with scala.collection.SetWrapper[A](set) {

    def +=(elem: A): Unit = set.+=(elem);

    override def incl(elems: A*): Unit = set.incl(elems);

    override def incl(that: Iterable[A]): Unit = set.incl(that);

    def -=(elem: A): Unit = set.-=(elem);

    override def excl(elems: A*): Unit = set.excl(elems);

    override def excl(that: Iterable[A]): Unit = set.excl(that);

    override def intersect(that: Set[A]): Unit = set.intersect(that);

    override def clear: Unit = set.clear;

    override def filter(p: A => Boolean): Unit =  set.filter(p);
}
