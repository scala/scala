/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.mutable;

/** This class implements single linked lists where both the head (<code>elem</code>)
 *  and the tail (<code>next</code>) are mutable.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
[serializable]
class LinkedList[A](head: A, tail: LinkedList[A])
  extends SingleLinkedList[A, LinkedList[A]]
{
  elem = head;
  next = tail;

  override def equals(obj: Any): Boolean = (
    obj.isInstanceOf[LinkedList[A]]
      && toList.equals((obj.asInstanceOf[LinkedList[A]]).toList)
  );

  override protected def stringPrefix: String = "LinkedList";
}
