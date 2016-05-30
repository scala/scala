/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package script

/** Classes that mix in the `Scriptable` class allow messages to be sent to
 *  objects of that class.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 09/05/2004
 *  @since   2.8
 */
@deprecated("scripting is deprecated", "2.11.0")
trait Scriptable[A] {
  /** Send a message to this scriptable object.
   */
  def <<(cmd: Message[A]): Unit
}
