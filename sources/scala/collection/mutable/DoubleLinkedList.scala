/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;


abstract class DoubleLinkedList[A, This <: DoubleLinkedList[A, This]]: This
	extends SingleLinkedList[A, This] {

	var prev: This = _;

	override def append(that: This): Unit =
		if (that == null)
			()
		else if (next == null) {
			next = that;
			that.prev = this;
		} else
			next.append(that);

	override def insert(that: This): Unit = if (that != null) {
		that.append(next);
		next = that;
		that.prev = this;
	}

	def remove: Unit = {
		if (next != null)
			next.prev = prev;
		if (prev != null)
			prev.next = next;
		prev = null;
		next = null;
	}
}
