/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;


class Queue[A] with MutableList[A] {

    def +=(elem: A) = appendElem(elem);

    def +=(iter: Iterable[A]) = iter.elements.foreach(e => appendElem(e));

    def enqueue(elems: A*): Unit = (this += elems);

    def dequeue(): A = {
    	if (first == null)
    		error("queue empty");
    	else {
    		val res = first.elem;
    		first = first.next;
    		res;
    	}
    }

    def clear: Unit = reset;
}
