/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;

class ListBuffer[A] with Seq[A] with PartialFunction[Int, A] {

    protected var first: LinkedList[A] = null;
    protected var last: LinkedList[A] = null;
    protected var len: Int = 0;

    def size: Int = len;

    def length: Int = len;

    def isDefinedAt(n: Int) = (n >= 0) && (n < len);

    def apply(n: Int): A = get(n) match {
        case None => null
        case Some(value) => value
    }

    def get(n: Int): Option[A] = first.get(n);

    def at(n: Int): A = apply(n);

    def remove(n: Int): A = {
        val old = apply(n);
        if (n >= len)
            error("cannot remove element " + n + " in ListBuffer");
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

    def clear: Unit = {
        first = null;
        last = null;
        len = 0;
    }

    def update(n: Int, newelem: A): Unit = {
        var elem = first;
        var i = n;
        while (i > 0) {
            elem = elem.next;
            if (elem == null)
                error("cannot update element " + n + " in ListBuffer");
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
                    error("cannot insert element " + n + " in ListBuffer");
                i = i - 1;
            }
            val old = elem.next;
            elem.next = new LinkedList[A];
            elem.next.elem = newelem;
            elem.next.next = old;
        }
    }

    def prepend(elem: A) = {
        if (len == 0) {
            first = new LinkedList[A];
            first.elem = elem;
            last = first;
        } else {
            val old = first;
            first = new LinkedList[A];
            first.elem = elem;
            first.next = old;
        }
        len = len + 1;
    }

    def prependAll(elem: A*) =
        (elem as List[A]).reverse.foreach(e => prepend(e));

    def prependSeq(iter: Iterable[A]) = prependIterator(iter.elements);

    protected def prependIterator(iter: Iterator[A]): Unit = if (iter.hasNext) {
        val cur = iter.next;
        prependIterator(iter);
        prepend(cur);
    }

    def +=(elem: A) = append(elem);

    def +=(elems: Iterable[A]) = appendSeq(elems);

    def append(elem: A) = {
        if (len == 0)
            prepend(elem);
        else {
            last.next = new LinkedList[A];
            last.next.elem = elem;
            last = last.next;
            len = len + 1;
        }
    }

    def appendAll(elem: A*) = (elem as List[A]).foreach(e => append(e));

    def appendSeq(iter: Iterable[A]) = iter.elements.foreach(e => append(e));

    def elements: Iterator[A] = first.elements;

    def toList: List[A] = first.toList;

    override def toString() = toList.toString();
}
