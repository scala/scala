/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection
package mutable

import generic._

/** Todo: this has O(n) cost for element removal.
 *  Should be rewritten to be more efficient.
 *  @since 2.2
 */
@serializable @SerialVersionUID(1L)
class LinkedHashSet[A] extends Set[A]
                          with GenericSetTemplate[A, LinkedHashSet]
                          with SetLike[A, LinkedHashSet[A]]
                          with FlatHashTable[A]
{
  override def companion: GenericCompanion[LinkedHashSet] = LinkedHashSet

  @transient private var ordered = new ListBuffer[A]

  override def size = tableSize

  def contains(elem: A): Boolean = containsEntry(elem)

  def += (elem: A): this.type = { add(elem); this }
  def -= (elem: A): this.type = { remove(elem); this }

  override def add(elem: A): Boolean =
    if (addEntry(elem)) { ordered += elem; true }
    else false

  override def remove(elem: A): Boolean =
    removeEntry(elem) match {
      case None => false
      case _ => ordered -= elem; true
    }

  override def clear() {
    ordered.clear()
    clearTable()
  }

  override def iterator = ordered.iterator

  override def foreach[U](f: A => U) = ordered foreach f

  private def writeObject(s: java.io.ObjectOutputStream) {
    serializeTo(s)
  }

  private def readObject(in: java.io.ObjectInputStream) {
    ordered = new ListBuffer[A]
    init(in, ordered += )
  }
}

/** Factory object for `LinkedHashSet` class */
object LinkedHashSet extends SetFactory[LinkedHashSet] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, LinkedHashSet[A]] = setCanBuildFrom[A]
  override def empty[A]: LinkedHashSet[A] = new LinkedHashSet[A]
}

