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
trait Map[A, +B] with Function1[A, B] with PartialFunction[A, B] {
	def size: Int;
    def isEmpty: Boolean;
	def get(key: A): Option[B];
	def remove(key: A): B;
	def contains(key: A): Boolean;
	def clear: Unit;
	def keys: Iterator[A];
	def values: Iterator[B];
	def iterator: Iterator[Pair[A, B]];
}
