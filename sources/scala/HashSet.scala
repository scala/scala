/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;

/** I promise, there will be some documentation soon! :-) Matthias
 */
class HashSet[A] extends Set[A] with HashTable[A] {

	def contains(elem: A): Boolean = findEntry(elem) match {
		case None => false
		case Some(_) => true
	}

	def add(elem: A): Unit = findEntry(elem) match {
		case None => addEntry(elem);
		case Some(_) =>
	}

	def remove(elem: A): Unit = removeEntry(elem);

  	def iterator = entries;

	protected type Entry = A;

	protected def entryKey(e: Entry) = e;
}
