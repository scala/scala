/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;


trait ImmutableSet[A, This <: ImmutableSet[A, This]]: This with Set[A] {

    def add(elem: A): This;

    def addAll(elems: A*): This = addSet(elems);

    def addSet(that: Iterable[A]): This = {
        var res = this;
        that.elements.foreach(elem => res = res.add(elem));
        res;
    }

    def remove(elem: A): This;

    def removeAll(elems: A*): This = removeSet(elems);

    def removeSet(that: Iterable[A]): This = {
        var res = this;
        that.elements.foreach(elem => res = res.remove(elem));
        res;
    }

    def intersect(that: Set[A]): This = filter(that.contains);

    def clear: This;

    def filter(p: A => Boolean): This = {
        var res = this;
        toList foreach {
            elem => if (p(elem)) { res = res.remove(elem); }
        }
        res;
    }
}
