/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.collection
package mutable

import generic._

/** This class implements mutable sets using a hashtable.
 *  The iterator and all traversal methods of this class visit elements in the order they were inserted.
 *
 *  $cannotStoreNull
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.0, 31/12/2006
 *  @since   1
 *
 *  @tparam A     the type of the elements contained in this set.
 *
 *  @define Coll `LinkedHashSet`
 *  @define coll linked hash set
 *  @define thatinfo the class of the returned collection. In the standard library configuration,
 *    `That` is always `LinkedHashSet[B]` because an implicit of type `CanBuildFrom[LinkedHashSet, B, LinkedHashSet[B]]`
 *    is defined in object `LinkedHashSet`.
 *  @define bfinfo an implicit value of class `CanBuildFrom` which determines the
 *    result class `That` from the current representation type `Repr`
 *    and the new element type `B`. This is usually the `canBuildFrom` value
 *    defined in object `LinkedHashSet`.
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 *  @define orderDependent
 *  @define orderDependentFold
 */
@SerialVersionUID(1L)
class LinkedHashSet[A] extends AbstractSet[A]
                          with Set[A]
                          with GenericSetTemplate[A, LinkedHashSet]
                          with SetLike[A, LinkedHashSet[A]]
                          with FlatHashTable[A]
                          with Serializable
{
  override def companion: GenericCompanion[LinkedHashSet] = LinkedHashSet

  @transient private[this] var ordered = new ListBuffer[A]

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

  override def iterator: Iterator[A] = ordered.iterator

  override def foreach[U](f: A => U) = ordered foreach f

  private def writeObject(s: java.io.ObjectOutputStream) {
    serializeTo(s)
  }

  private def readObject(in: java.io.ObjectInputStream) {
    ordered = new ListBuffer[A]
    init(in, ordered += _)
  }
}

/** $factoryInfo
 *  @define Coll `LinkedHashSet`
 *  @define coll linked hash set
 */
object LinkedHashSet extends MutableSetFactory[LinkedHashSet] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, LinkedHashSet[A]] = setCanBuildFrom[A]
  override def empty[A]: LinkedHashSet[A] = new LinkedHashSet[A]
}

