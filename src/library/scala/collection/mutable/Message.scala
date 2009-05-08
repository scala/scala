/* TODO: Reintegrate
/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable


import Predef._

/** Class <code>Message</code> represents messages that are issued by observable
 *  collection classes whenever a data structure is changed. Class <code>Message</code>
 *  has several subclasses for the various kinds of events: <code>Update</code>
 *  <code>Remove</code>, <code>Include</code>, <code>Reset</code>, and
 *  <code>Script</code>.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
trait Message[+A]

/** This observable update refers to inclusion operations that add new elements
 *  to collection classes.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
case class Include[+I](elem: I) extends Message[I]

/** This observable update refers to destructive modification operations
 *  of elements from collection classes.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
case class Update[+A](elem: A) extends Message[A]

/** This observable update refers to removal operations of elements
 *  from collection classes.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
case class Remove[+A](elem: A) extends Message[A]

/** This command refers to reset operations.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
case class Reset[+A]() extends Message[A]

/** Objects of this class represent compound messages consisting
 *  of a sequence of other messages.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 10/05/2004
 */
class Script[A] extends ArrayBuffer[Message[A]] with Message[A] {

  override def toString(): String = {
    var res = "Script("
    var it = elements
    var i = 1
    while (it.hasNext) {
      if (i > 1)
        res = res + ", "
      res = res + "[" + i + "] " + it.next
      i = i + 1
    }
    res + ")"
  }

  override def hashCode(): Int =
    throw new UnsupportedOperationException("scripts are not suitable as hash keys")
}
*/
