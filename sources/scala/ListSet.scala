/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala;

/** I promise, there will be some documentation soon! :-) Matthias
 */
class ListSet[A] extends MutableSet[A] {

    protected var elems: List[A] = Nil;

    def size: Int = elems.length;

    def contains(elem: A): Boolean = elems.contains(elem);

    def add(elem: A): Unit = if (!elems.contains(elem)) elems = elem :: elems;

    def remove(elem: A): Unit = { elems = elems.filter(e => e != elem); }

    def clear: Unit = { elems = Nil; }

    def elements: Iterator[A] = elems.elements;

    override def toList: List[A] = elems;

}
