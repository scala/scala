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

    def add(elem: A): Unit;

    def addAll(elems: A*): Unit = {
        val ys = elems as List[A];
        ys foreach { y => add(y); };
    }

    def addSet(that: Iterable[A]): Unit =
        that.elements.foreach(elem => add(elem));

    def remove(elem: A): Unit;

    def removeAll(elems: A*): Unit = removeSet(elems);

    def removeSet(that: Iterable[A]): Unit =
        that.elements.foreach(elem => remove(elem));

    def intersect(that: Set[A]): Unit = filter(that.contains);

    def clear: Unit;

    def filter(p: A => Boolean): Unit = toList foreach {
        elem => if (p(elem)) remove(elem);
    }
}
