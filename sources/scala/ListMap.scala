/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala;


class ListMap[A, B] extends MutableMap[A, B] with DefaultMapModel[A, B] {

    var xs: List[Entry] = Nil;

    def size: Int = xs.length;

    override def clear: Unit = { xs = Nil; }

    protected def findEntry(key: A) = xs find {e => e.key == key};

    protected def addEntry(e: Entry) = { xs = e :: xs; }

    def remove(key: A) = { xs = xs filter {e => e.key != key}; }

    protected def entries = xs.elements;

}
