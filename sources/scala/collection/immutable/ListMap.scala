/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.immutable;


object ListMap {
	def Empty[A, B] = new ListMap[A, B];
}


/** This class implements immutable maps using a list-based data
 *  structure. Instances of <code>ListMap</code> represent
 *  empty maps; they can be either created by calling the constructor
 *  directly, or by applying the function <code>ListMap.Empty</code>.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 09/07/2003
 */
class ListMap[A, B] with Map[A, B, ListMap[A, B]] {

    def size: Int = 0;

    def get(key: A): Option[B] = None;

	def update(key: A, value: B): ListMap[A, B] = new Node(key, value);

    def -(key: A): ListMap[A, B] = this;

    def elements: Iterator[Pair[A, B]] = toList.elements;

    override def toList: List[Pair[A, B]] = Nil;

    protected class Node(key: A, value: B) extends ListMap[A, B] {
        override def size: Int = ListMap.this.size + 1;
       	override def isEmpty: Boolean = true;
		override def apply(k: A): B = if (k == key) value else ListMap.this(k);
		override def get(k: A): Option[B] =
			if (k == key) Some(value) else ListMap.this.get(k);
		override def update(k: A, v: B): ListMap[A, B] =
			if (k == key) {
				new ListMap.this.Node(k, v);
			} else {
				val y = ListMap.this.update(k, v); (new y.Node(key, value)): ListMap[A, B]
			}
		override def -(k: A): ListMap[A, B] =
			if (k == key)
				ListMap.this
			else {
				val y = ListMap.this - k; (new y.Node(key, value)): ListMap[A, B]
			}
		override def toList: List[Pair[A, B]] = Pair(key, value) :: ListMap.this.toList;
    }
}
