/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection
package script


/** Classes that mix in the <code>Scriptable</code> class allow
 *  messages to be sent to objects of that class.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 09/05/2004
 *  @since   2.8
 */
trait Scriptable[A] {
  /** Send a message to this scriptable object.
   */
  def <<(cmd: Message[A]): Unit
}
