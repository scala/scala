/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable

/** This class implements mutable sets using a hashtable.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.0, 31/12/2006
 */
object HashSet {

  /** The empty map of this type */
  def empty[A] = new HashSet[A]

  /** The canonical factory for this type
   */
  def apply[A](elems: A*) = empty[A] ++ elems
}

@serializable
class HashSet[A] extends Set[A] with FlatHashTable[A] {

  def contains(elem: A): Boolean = containsEntry(elem)

  def +=(elem: A) { addEntry(elem) }

  def -=(elem: A) { removeEntry(elem) }

  override def clear() = super.clear()

  override def clone(): Set[A] = new HashSet[A] ++ this
}

