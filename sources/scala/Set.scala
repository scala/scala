/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;

/** I promise, there will be some documentation soon! :-) Matthias
 */
trait Set[A] with Iterable[A] {

    def size: Int;

    def isEmpty: Boolean = (size == 0);

    def contains(elem: A): Boolean;

    def add(elem: A): Unit;

    def addAll(elems: A*): Unit = {
        val ys = elems as List[A];
        ys foreach { y => add(y); };
    }

    def addSet(that: Iterable[A]): Unit =
        that.elements.foreach(elem => add(elem));

    def remove(elem: A): Unit;

    def removeAll(elems: A*): Unit = {
        val ys = elems as List[A];
        ys foreach { y => remove(y); };
    }

    def removeSet(that: Iterable[A]): Unit =
        that.elements.foreach(elem => remove(elem));

    def intersect(that: Set[A]): Unit = filter(that.contains);

    def clear: Unit;

    def subsetOf(that: Set[A]): Boolean = {
        val iter = elements;
        var res = true;
        while (res && iter.hasNext) {
            res = that.contains(iter.next);
        }
        res
    }

    def foreach(f: A => Unit) = {
        val iter = elements;
        while (iter.hasNext) {
            f(iter.next);
        }
    }

    def filter(p: A => Boolean): Unit = toList foreach {
        elem => if (p(elem)) remove(elem);
    }

    def toList: List[A] = {
        var res: List[A] = Nil;
        val iter = elements;
        while (iter.hasNext) {
            res = iter.next :: res;
        }
        res;
    }

    override def toString() =
        if (size == 0)
            "{}"
        else
            "{" + {
                val iter = elements;
                var res = iter.next.toString();
                while (iter.hasNext) {
                    res = res + ", " + iter.next;
                }
                res;
            } + "}";
}
