/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala;


trait DefaultMapModel[A, B] extends MutableMap[A, B] {

    protected def findEntry(key: A): Option[Entry];

    protected def addEntry(e: Entry): Unit;

    protected def removeEntry(key: A): Unit;

    protected def entries: Iterator[Entry];

    def get(key: A) = findEntry(key) match {
        case None => None
        case Some(e) => Some(e.value);
    }

    def update(key: A, value: B) = findEntry(key) match {
        case None => addEntry(new Entry(key, value));
        case Some(e) => e.value = value;
    }

    def remove(key: A) = findEntry(key) match {
    	case None => null;
    	case Some(e) => removeEntry(key); e.value;
    }

    def elements = new Iterator[Pair[A, B]] {
        val iter = entries;
        def hasNext = iter.hasNext;
        def next = iter.next.toPair;
    }

    protected class Entry(k: A, v: B) {
        def key = k;
        var value = v;
        def toPair = Pair(k, value);
        override def toString() = k.toString() + " -> " + value;
    }

}
