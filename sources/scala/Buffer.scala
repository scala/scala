/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;


class Buffer[A] with MutableList[A] {

    def prepend(elem: A) = prependElem(elem);

    def append(elems: A*) = (this += elems);

    def +=(elem: A) = appendElem(elem);

    def +=(iter: Iterable[A]) = iter.elements.foreach(e => appendElem(e));

    def update(n: Int, newelem: A): Unit = {
        var elem = first;
        var i = n;
        while (i > 0) {
            elem = elem.next;
            if (elem == null)
                error("cannot update element " + n + " in Buffer");
            i = i - 1;
        }
        elem.elem = newelem;
    }

    def insert(n: Int, newelem: A): Unit = {
        if (n == 0)
            prepend(newelem);
        else if (n >= len)
            append(newelem);
        else {
            var elem = first;
            var i = n;
            while (i > 1) {
                elem = elem.next;
                if (elem == null)
                    error("cannot insert element " + n + " in Buffer");
                i = i - 1;
            }
            val old = elem.next;
            elem.next = new LinkedList[A];
            elem.next.elem = newelem;
            elem.next.next = old;
        }
    }

    def remove(n: Int): A = {
        val old = apply(n);
        if (n >= len)
            error("cannot remove element " + n + " in Buffer");
        if ((n == 0) && (len == 1)) {
            first = null;
            last = null;
        } else if (n == 0) {
            first = first.next;
        } else {
            var elem = first;
            var i = n;
            while (i > 1) {
                elem = elem.next;
                i = i - 1;
            }
            elem.next = elem.next.next;
            if (n == (len - 1)) {
                last = elem.next;
            }
        }
        len = len - 1;
        old;
    }

    def clear: Unit = reset;
}
