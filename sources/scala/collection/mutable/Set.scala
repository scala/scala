/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.mutable;


/** This trait represents mutable sets. Concrete set implementations
 *  just have to provide functionality for the abstract methods in
 *  <code>scala.collection.Set</code> as well as for <code>add</code>,
 *  <code>remove</code>, and <code>clear</code>.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
trait Set[A] with scala.collection.Set[A] {

    def +=(elem: A): Unit;

    def incl(elems: A*): Unit = {
        val ys = elems.asInstanceOf[List[A]];
        ys foreach { y => +=(y); };
    }

    def incl(that: Iterable[A]): Unit =
        that.elements.foreach(elem => +=(elem));

    def -=(elem: A): Unit;

    def excl(elems: A*): Unit = excl(elems);

    def excl(that: Iterable[A]): Unit =
        that.elements.foreach(elem => -=(elem));

    def intersect(that: Set[A]): Unit = filter(that.contains);

    def clear: Unit;

    def filter(p: A => Boolean): Unit = toList foreach {
        elem => if (p(elem)) -=(elem);
    }
}
