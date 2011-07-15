/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package mutable

/** `Subscriber[A, B]` objects may subscribe to events of type `A`
 *  published by an object of type `B`. `B` is typically a subtype of
 *  [[scala.collection.immutable.Publisher]].
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   1
 */
trait Subscriber[-Evt, -Pub] {
  def notify(pub: Pub, event: Evt): Unit
}
