/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable

/** This class implements single linked lists where both the head (<code>elem</code>)
 *  and the tail (<code>next</code>) are mutable.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
@serializable
class LinkedList[A](var elem: A, var next: LinkedList[A])
  extends SingleLinkedList[A, LinkedList[A]]
{

  /** Compares two lists structurally; i.e. checks if all elements
   *  contained in this list are also contained in the other list,
   *  and vice versa.
   *
   *  @param that the other list
   *  @return     <code>true</code> iff both lists contain exactly the
   *              same mappings.
   */
  override def equals(obj: Any): Boolean = obj match {
    case that: LinkedList[_] => this.toList equals that.toList
    case _ => false
  }

  /** A hash method compatible with <code>equals</code>
   */
  override def hashCode(): Int =
    (0 /: elements) ((hash, kv) => hash + kv.hashCode)

  override protected def stringPrefix: String = "LinkedList"
}



