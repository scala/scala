/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.immutable;


/** This trait represents immutable sets. Concrete set implementations
 *  just have to provide functionality for the abstract methods in
 *  <code>scala.collection.Set</code> as well as for <code>+</code> and
 *  <code>-</code>.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 19/07/2003
 */
trait Set[A] with scala.collection.Set[A] {

    def +(elem: A): Set[A];

    def incl(elems: A*): Set[A] = incl(elems);

    def incl(that: Iterable[A]): Set[A] = {
        var res = this;
        that.elements.foreach(elem => res = res + elem);
        res;
    }

    def -(elem: A): Set[A];

    def excl(elems: A*): Set[A] = excl(elems);

    def excl(that: Iterable[A]): Set[A] = {
        var res = this;
        that.elements.foreach(elem => res = res - elem);
        res;
    }

    def intersect(that: scala.collection.Set[A]): Set[A] = filter(that.contains);

    def filter(p: A => Boolean): Set[A] = {
        var res = this;
        toList foreach {
            elem => if (p(elem)) { res = res - elem; }
        }
        res;
    }
}
