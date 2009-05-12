/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.mutable

import generic._

//!!! todo make inherit from HashSet.
object LinkedHashSet {
  /** The empty map of this type */
  def empty[A] = new LinkedHashSet[A]

  /** The canonical factory for this type
   */
  def apply[A](elems: A*) = empty[A] ++ collection.Iterable.fromOld(elems)
}

@serializable
class LinkedHashSet[A] extends Set[A] with MutableSetTemplate[A, LinkedHashSet[A]] with FlatHashTable[A] {

  override def empty = LinkedHashSet.empty

  override def size = super.size

  private var ordered = List[A]()

  def contains(elem: A): Boolean = containsEntry(elem)

  def += (elem: A): this.type = { put(elem); this }
  def -= (elem: A): this.type = { remove(elem); this }

  override def put(elem: A): Boolean =
    if (addEntry(elem)) {
      ordered = elem :: ordered
      true
    } else false

  override def remove(elem: A): Boolean = removeEntry(elem) match {
    case None => false
    case Some(elem) => ordered = ordered.filter(_ != elem); true
  }

  override def clear() {
    ordered = Nil
    super.clear()
  }

  override def elements = Iterator.fromOld(ordered.reverse.elements)
}

