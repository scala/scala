/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala;


abstract class SingleLinkedList[A, This <: SingleLinkedList[A, This]]: This with Seq[A] {

    var elem: A = _;

    var next: This = _;

    def length: Int = 1 + (if (next == null) 0 else next.length);

    def append(that: This): Unit =
        if (next == null) { next = that; } else next.append(that);

    def insert(that: This): Unit = if (that != null) {
        that.append(next);
        next = that;
    }

    def apply(n: Int): A = {
        if (n == 0) elem
        else if (next == null) null
        else next.apply(n - 1);
    }

    def at(n: Int): A = apply(n);

    def get(n: Int): Option[A] = {
        if (n == 0) Some(elem)
        else if (next == null) None
        else next.get(n - 1);
    }

    def elements: Iterator[A] = new Iterator[A] {
        var elems = SingleLinkedList.this;
        def hasNext = (elems != null);
        def next = {
            val res = elems.elem;
            elems = elems.next;
            res;
        }
    }

    def toList: List[A] =
        if (next == null) (elem :: Nil) else (elem :: next.toList);

}
