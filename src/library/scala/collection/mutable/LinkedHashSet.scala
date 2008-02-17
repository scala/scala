/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.mutable


object LinkedHashSet {
  /** The empty map of this type */
  def empty[A] = new LinkedHashSet[A]

  /** The canonical factory for this type
   */
  def apply[A](elems: A*) = empty[A] ++ elems
}

@serializable
class LinkedHashSet[A] extends Set[A] with FlatHashTable[A] {
  private var ordered = List[A]()

  def contains(elem: A): Boolean = containsEntry(elem)

  def +=(elem: A) { add(elem) }

  def add(elem : A) : Boolean = {
    if (addEntry(elem)) {
      ordered = elem :: ordered
      true
    } else false
  }
  def -=(elem: A) { remove(elem) }
  def remove(elem : A) : Boolean = removeEntry(elem) match {
  case None => false
  case Some(elem) => ordered = ordered.filter(e => e != elem); true
  }

  override def clear() = {
    ordered = Nil
    super.clear()
  }
  override def clone(): Set[A] = new LinkedHashSet[A] ++ this
  override def elements = ordered.reverse.elements
}
