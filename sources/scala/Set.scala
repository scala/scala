/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;


trait Set[A] with Iterable[A] {

    def size: Int;

    def isEmpty: Boolean = (size == 0);

    def contains(elem: A): Boolean;

    def subsetOf(that: Set[A]): Boolean = {
        val iter = elements;
        var res = true;
        while (res && iter.hasNext) {
            res = that.contains(iter.next);
        }
        res
    }

    def foreach(f: A => Unit): Unit = {
        val iter = elements;
        while (iter.hasNext) {
            f(iter.next);
        }
    }

    def exists(p: A => Boolean): Boolean = {
    	val iter = elements;
    	var res = false;
        while (!res && iter.hasNext) {
            if (p(iter.next)) { res = true; }
        }
        res;
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
