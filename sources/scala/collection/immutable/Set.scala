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
trait Set[A, +This <: Set[A, This]]: This with scala.collection.Set[A] {

    def +(elem: A): This;

    def incl(elems: A*): This = incl(elems);

    def incl(that: Iterable[A]): This = {
        var res = this;
        that.elements.foreach(elem => res = res + elem);
        res;
    }

    def -(elem: A): This;

    def excl(elems: A*): This = excl(elems);

    def excl(that: Iterable[A]): This = {
        var res = this;
        that.elements.foreach(elem => res = res - elem);
        res;
    }

    def intersect(that: scala.collection.Set[A]): This = filter(that.contains);

    def filter(p: A => Boolean): This = {
        var res = this;
        toList foreach {
            elem => if (p(elem)) { res = res - elem; }
        }
        res;
    }
}
