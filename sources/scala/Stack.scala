/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;


class Stack[A] with MutableList[A] {

    def +=(elem: A) = prependElem(elem);

    def +=(iter: Iterable[A]) = iter.elements.foreach(e => prependElem(e));

    def push(elems: A*): Unit = (this += elems);

    def top: A = if (first == null) error("stack empty"); else first.elem;

    def pop: Unit = if (first != null) { first = first.next; }

    def clear: Unit = reset;

    override def elements: Iterator[A] = toList.elements;

    override def toList: List[A] = super.toList.reverse;
}
