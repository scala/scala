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
trait MutableMap[A, B] extends Map[A, B] {
	def update(key: A, value: B): Unit;
	def put(key: A, value: B): B;
}
