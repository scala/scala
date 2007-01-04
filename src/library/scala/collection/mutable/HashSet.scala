/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
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

[serializable]
class HashSet[A] extends Set[A] with HashTable[A] {

  def contains(elem: A): Boolean = findEntry(elem) != null

  def +=(elem: A) { if (findEntry(elem) == null) addEntry(new SetEntry(elem)) }

  def -=(elem: A) { removeEntry(elem) }

  def elements = entries map (.key)

  def clear {initTable(); tableSize = 0 }

  protected type Entry = SetEntry[A]

  override def clone(): Set[A] = new HashSet[A] ++ this
}

[serializable]
final class SetEntry[A](val key: A) extends HashEntry[A, SetEntry[A]]
