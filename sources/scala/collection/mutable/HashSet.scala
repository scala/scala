/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.mutable;


/** This class implements mutable sets using a hashtable.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
class HashSet[A] extends scala.collection.mutable.Set[A] with HashTable[A] {

    def contains(elem: A): Boolean = findEntry(elem) match {
        case None => false
        case Some(_) => true
    }

    def +=(elem: A): Unit = findEntry(elem) match {
        case None => addEntry(elem);
        case Some(_) =>
    }

    def -=(elem: A): Unit = removeEntry(elem);

    def elements = entries;

    override def clear = {
        initTable(table.length - 1);
        tableSize = 0;
    }

    protected type Entry = A;

    protected def entryKey(e: Entry) = e;
}
